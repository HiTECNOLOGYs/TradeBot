(in-package :tradebot)

(defun calculate-profit (amount sell buy)
  (let* ((sell-profit (* amount sell
                         (/ (- 100 +fee+) 100)))
         (buy-amount  (* (/ sell-profit buy)
                         (/ (- 100 +fee+) 100))))
    (- buy-amount amount)))

(defun open-order (order)
  (let ((response
         (send-trade-api-request "Trade"
                                 "pair" (order-pair order)
                                 "type" (order-type order)
                                 "rate" (order-rate order)
                                 "amount" (order-amount order))))
    (setf (order-id order) (jsown:val response "order_id"))))

(defun count-orders-types (orders-history)
  (loop for order in orders-history
        if (equal (order-type order) "sell")
          sum 1 into sell-count
        else if (equal (order-type order) "buy")
          sum 1 into buy-count
        finally (return (list :buy buy-count :sell sell-count))))

(defun get-orders-types-ratio (orders-types-count)
  (/ (getf orders-types-count :buy)
     (getf orders-types-count :sell)))
