(in-package :tradebot)

(defun fee- (&rest numbers)
  (apply #'* (/ (- 100 +fee+) 100)
         numbers))

(defun calculate-profit (amount rate last-balance)
  (let* ((new-balance (fee- amount rate)))
    (- new-balance last-balance)))

(defun open-order (order)
  (let ((response
         (send-trade-api-request "Trade"
                                 "pair" (order-pair order)
                                 "type" (order-type order)
                                 "rate" (write-to-string (order-rate order))
                                 "amount" (write-to-string (order-amount order)))))
    (setf (order-id order) (jsown:val response "order_id"))))

(defun %get-pair (coin)
  (case coin
    (:ltc "ltc_usd")
    (:btc "btc_usd")
    (otherwise "Dunno such coin: ~S" coin)))

(defun sell-all (coin)
  (let ((balance (getf (get-balance) coin))
        (rate (get-current-sell-price coin)))
    (open-order (make-order "sell"
                            (%get-pair coin)
                            rate
                            balance))))

(defun buy-for-all (coin)
  (let ((balance (getf (get-balance) :usd))
        (rate (get-current-buy-price coin)))
    (open-order (make-order "buy"
                            (%get-pair coin)
                            rate
                            (/ (floor balance) rate)))))

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
