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
                                 "rate" (format nil "~F" (order-rate order))
                                 "amount" (format nil "~F" (order-amount order)))))
    (setf (order-id order) (jsown:val response "order_id"))
    order))

(defun %get-pair (coin)
  (case coin
    (:ltc "ltc_usd")
    (:btc "btc_usd")
    (otherwise (error "Dunno such coin: ~S" coin))))

(defun sell-all (coin &optional rate)
  (let ((balance (getf (get-balance) coin))
        (rate (or rate (get-current-sell-price coin))))
    (open-order (make-order "sell"
                            (%get-pair coin)
                            rate
                            balance))))

(defun buy-for-all (coin &optional rate)
  (let ((balance (getf (get-balance) :usd))
        (rate (or rate (get-current-buy-price coin))))
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

(defun get-current-profit (coin operation)
  (let* ((orders (get-trades-history))
         (last-balance (get-last-balance orders))
         (rate (case operation
                 (:buy (get-current-buy-price coin))
                 (:sell (get-current-sell-price coin))
                 (otherwise (error "Dunno such operation: ~S" operation)))))
    (values (calculate-profit (getf (get-balance) coin)
                              rate
                              last-balance)
            rate)))

(defun get-current-orders-ratio (coin)
  (get-orders-types-ratio (count-orders-types (get-global-trades-history coin))))
