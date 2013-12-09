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
