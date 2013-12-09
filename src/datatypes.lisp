(in-package :tradebot)

(defclass Order ()
  ((id :initarg :id
       :accessor order-id)
   (type :initarg :type
         :reader order-type)
   (pair :initarg :pair
         :reader order-pair)
   (rate :initarg :rate
         :reader order-rate)
   (amount :initarg :amount
           :reader order-amount)))

(defun make-order (type pair rate amount &optional id)
  (make-instance 'Order
                 :id id
                 :type type
                 :pair pair
                 :rate rate
                 :amount amount))

(defun format-orders (orders)
  (format t "~%~{~{~A~5T | ~A~20T | ~A~%~}~}"
          (loop for order in orders
                collecting (list (order-type order)
                                 (coerce (order-rate order) 'float)
                                 (coerce (order-amount order) 'float)))))
