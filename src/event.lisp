;; This file is part of Until It Dies

;; event.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

;;;
;;; Event prototype
;;;
(defproto =event= ()
  ((payload (lambda () (print "=event='s event fired.")))
   (exec-time 0))
  (:documentation 
"An event is a  block of code that is executed by an event-queue.
Event firing can be delayed by milliseconds, and they are guaranteed
to not fire until they are 'cooked'.
Events can be asynchronously added to an event-queue, which can 
remain inactive until execution is ready to start again, and they
will still be 'fired' in the order they were added."))

(defvar *event-queue*)
(defun make-event (payload &key 
		   (queue *event-queue*)
		   (delay 0)
		   (event-prototype =event=))
  "Generates one or more events that execute PAYLOAD."
  (push-event (defobject (event-prototype)
                  ((payload payload)
                   (exec-frame (+ delay (now)))))
	      queue))

;; TODO: FORK was simplified. I may need to add looping to it again. Keep it like this for now.
(defmacro fork ((&key queue delay) 
		&body body)
  "Turns BODY into a function to be added as a payload to an =event= object, which
   will be delayed by DELAY milliseconds, and added to QUEUE."
  `(make-event (lambda () ,@body)
	       ,@(when queue `(:queue ,queue))
	       ,@(when delay `(:delay ,delay))))

(defmacro with-event-queue (queue &body body)
  `(let ((*event-queue* ,queue))
     ,@body))

;;;
;;; Event processing buzzwords
;;;
(defmessage execute-event (event)
  (:documentation
"Takes care of executing a particular event."))

(defmessage cookedp (event)
  (:documentation
"Is the event ready to fire?"))

;;; Messages
(defreply execute-event ((event =event=))
  "Executes a standard event. Nothing fancy, just a funcall."
  (funcall (payload event)))

(defreply cookedp ((event =event=))
  "Simply checks that it doesn't shoot its load prematurely.."
  (let  ((time-difference (- (exec-time event) (now))))
    (when (<= time-difference 0)
      t)))

;;;
;;; Event-queue
;;;
(defproto =event-queue= ()
  ((queue (make-priority-queue :key #'exec-time) 
	  :cloneform (make-priority-queue :key #'exec-time)))
  (:documentation "An event queue is a container for events. Events are inserted into it and
                   automatically sorted according to the event's execution time. The queue
                   works like a min-priority queue. The top event can be peeked at, or popped."))

;;; Buzzwords
(defmessage push-event (event queue)
  (:documentation "Adds EVENT to QUEUE"))
(defmessage peek-next-event (queue)
  (:documentation "Peeks at the next event in QUEUE without removing it.
                   Returns NIL if there are no queued events."))
(defmessage pop-next-event (queue)
  (:documentation "Returns the next available event, and removes it from QUEUE.
                   Returns NIL if there is nothing in the queue."))
(defmessage event-available-p (queue)
  (:documentation "Is there a cooked event available?"))
(defmessage clear-events (queue)
  (:documentation "Clears all events off the event queue"))
(defmessage process-next-event (queue)
  (:documentation "Grabs the next event from QUEUE and executes it."))
(defmessage process-cooked-events (queue)
  (:documentation "Processes all cooked events in QUEUE"))

;;; Messages
;;;
;;; - Most of these messages wrap around the priority queue API in util/priority-queue.lisp
;;;   It is pretty much a standard by-the-book min-priority-queue, with items sorted by
;;;   #'exec-time.
(defreply push-event ((event =event=) (queue =event-queue=))
  (priority-queue-insert (queue queue) event)
  t)

(defreply peek-next-event ((queue =event-queue=))
  (priority-queue-minimum (queue queue)))

(defreply pop-next-event ((queue =event-queue=))
  (priority-queue-extract-minimum (queue queue)))

(defreply event-available-p ((queue =event-queue=))
  "Simply peeks to see if there's an event in the queue, and if it's cooked."
  (when (and (peek-next-event queue)
	     (cookedp (peek-next-event queue)))
    t))

(defreply clear-events ((queue =event-queue=))
  (setf (queue queue) (make-priority-queue :key #'exec-time)))

(defreply process-next-event ((queue =event-queue=))
  (when (event-available-p queue)
    (execute-event (pop-next-event queue))))

(defreply process-cooked-events ((queue =event-queue=))
  (loop
     while (event-available-p queue)
     do (process-next-event queue)))

