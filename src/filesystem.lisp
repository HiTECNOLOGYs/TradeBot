(in-package :tradebot)

(defun save-data ()
  (cl-store:store (list *nonce*)
                  #p"~/Documents/trade_bot.db"))

(defun load-data ()
  (destructuring-bind (nonce)
      (cl-store:restore #p"~/Documents/trade_bot.db")
    (setf *nonce* nonce)))
