(in-package :tradebot)

(defclass Order ()
  ((type :initarg :type
         :reader order-type)
   (pair :initarg :pair
         :reader order-pair)
   (rate :initarg :rate
         :reader order-rate)
   (amount :initarg :amount
           :reader order-amount)))

(defun make-order (type pair rate amount)
  (make-instance 'Order
                 :type type
                 :pair pair
                 :rate rate
                 :amount amount))
