(in-package :tradebot)

;;; ********************************************************************
;;;  Utility-functions
;;; ********************************************************************

(defun avg (list &optional key)
  (/ (reduce #'+ list :key key)
     (length list)))


;;; ********************************************************************
;;;  Basics
;;; ********************************************************************

(defvar *nonce* 0)

(defun get-nonce ()
  (prog1 (write-to-string *nonce*)
    (incf *nonce*)))

(defun hmac-sha512 (s)
  (let* ((s (ironclad:ascii-string-to-byte-array s))
         (key (ironclad:ascii-string-to-byte-array +api-secret+))
         (hmac (ironclad:make-hmac key 'ironclad:sha512)))
    (ironclad:update-hmac hmac s)
    (ironclad:byte-array-to-hex-string (ironclad:hmac-digest hmac))))

(defun make-post-data-signature (data)
  (let* ((formated-data (let ((*print-case* :downcase))
                          (format nil "~{~A=~A~#,1,1^&~}" data))))
    (hmac-sha512 formated-data)))

(defun send-trade-api-request (method &rest data)
  (let* ((nonce (get-nonce))
         (signature (make-post-data-signature (append (list "nonce" nonce "method" method)
                                                      data)))
         (response (drakma:http-request (get-api-url :trade) :method :post
                                        :additional-headers
                                        (list (cons "Key" +api-key+)
                                              (cons "Sign" signature))
                                        :parameters
                                        (append (list (cons "nonce" nonce)
                                                      (cons "method" method))
                                                (alexandria:plist-alist data))))
         (parsed (jsown:parse response)))
    (values (jsown:val parsed "return")
            (when (= (jsown:val parsed "success") 1)
              t))))


;;; ********************************************************************
;;;  High-level wrappers
;;; ********************************************************************

(defun get-graph (coin)
  (let ((response (drakma:http-request (get-info-api-url coin :graph))))
    (jsown:val (jsown:parse response) "asks")))

(defun get-ticker (coin)
  (let ((response (drakma:http-request (get-info-api-url coin :ticker))))
    (jsown:val (jsown:parse response) "ticker")))

(defun get-global-trades-history (coin)
  (let ((response (jsown:parse (drakma:http-request (get-info-api-url coin :history)))))
    (loop for order in response
          for type = (jsown:val order "trade_type")
          for item = (jsown:val order "item")
          for price-currency = (jsown:val order "price_currency")
          collecting (make-order (cond ((equal "bid" type) "sell")
                                       ((equal "ask" type) "buy"))
                                 (let ((*print-case* :downcase))
                                   (format nil "~A_~A" item price-currency))
                                 (jsown:val order "price")
                                 (jsown:val order "amount")
                                 (jsown:val order "tid")))))

(defun get-trades-history ()
  (let ((response (send-trade-api-request "TradeHistory")))
    (loop for tid in (jsown:keywords response)
          for order = (jsown:val response tid)
          collecting (make-order (jsown:val order "type")
                                 (jsown:val order "pair")
                                 (coerce (jsown:val order "rate") 'float)
                                 (coerce (jsown:val order "amount") 'float)
                                 tid))))

(defun get-last-order (orders-history)
  (labels
      ((iter (list acc)
         (if (not (equal (order-type (first acc)) (order-type (first list))))
             acc
             (iter (rest list)
                   (cons (first list) acc)))))
    (iter (rest orders-history)
          (list (first orders-history)))))

(defun get-last-buy-order (orders-history)
  (unless (endp orders-history)
    (if (equal "buy" (order-type (first orders-history)))
        (get-last-order orders-history)
        (get-last-buy-order (rest orders-history)))))

(defun get-last-sell-order (orders-history)
  (unless (endp orders-history)
    (if (equal "sell" (order-type (first orders-history)))
        (get-last-order orders-history)
        (get-last-sell-order (rest orders-history)))))

(defun get-last-sell-price (orders-history)
  (let ((order (get-last-sell-order orders-history)))
    (avg order #'order-rate)))

(defun get-last-buy-price (orders-history)
  (let ((order (get-last-buy-order orders-history)))
    (avg order #'order-rate)))

(defun get-last-sell-amount (orders-history)
  (let ((order (get-last-sell-order orders-history)))
    (reduce #'+ order :key #'order-amount)))

(defun get-last-buy-amount (orders-history)
  (let ((order (get-last-buy-order orders-history)))
    (reduce #'+ order :key #'order-amount)))

(defun get-last-balance (orders-history)
  (let ((last-operation (order-type (first orders-history))))
    (cond
      ((equal last-operation "sell") (* (get-last-sell-amount orders-history)
                                        (get-last-sell-price orders-history)))
      ((equal last-operation "buy")  (* (get-last-buy-amount orders-history)
                                        (get-last-buy-price orders-history)))
      (t (error "Unknown order type: ~S" last-operation)))))

(defun get-balance ()
  (let ((funds (jsown:val (send-trade-api-request "getInfo") "funds")))
    (list :usd (coerce (jsown:val funds "usd") 'float)
          :btc (coerce (jsown:val funds "btc") 'float)
          :ltc (coerce (jsown:val funds "ltc") 'float))))

(defun get-current-sell-price (coin)
  (let ((response (get-ticker coin)))
    (coerce (jsown:val response "sell") 'float)))

(defun get-current-buy-price (coin)
  (let ((response (get-ticker coin)))
    (coerce (jsown:val response "buy") 'float)))

