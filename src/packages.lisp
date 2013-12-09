(defpackage :tradebot
  (:use :cl)
  (:export start)
  (:import-from :alexandria
                :define-constant
                :plist-alist))

(in-package :tradebot)

(defparameter *api*
  '((:trade . "https://btc-e.com/tapi")
    (:ltc (:ticker "https://btc-e.com/api/2/ltc_usd/ticker")
          (:graph "https://btc-e.com/api/2/ltc_usd/depth")
          (:history "https://btc-e.com/api/2/ltc_usd/trades"))
    (:btc (:ticker "https://btc-e.com/api/2/btc_usd/ticker")
          (:graph "https://btc-e.com/api/2/btc_usd/depth")
          (:history "https://btc-e.com/api/2/btc_usd/trades"))))

(defun get-api-url (id)
  (cdr (assoc id *api*)))

(defun get-info-api-url (coin id)
  (second (assoc id (get-api-url coin))))

(define-constant +fee+ 0.2)

(define-constant +api-key+ "99AHJ536-JLF06W2L-975HI9FI-1S5G9FI5-Y1BPQLZK"
  :test #'equal)
(define-constant +api-secret+ "e8baa267afe0cc83d82a3b3cf1c680a43df472d263de7618f539c70fc597c9f0"
  :test #'equal)
