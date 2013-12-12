(in-package :tradebot)

(defun save-data ()
  (cl-store:store (list *nonce* *history*)
                  #p"~/Documents/trade_bot.db"))

(defun load-data ()
  (destructuring-bind (nonce history)
      (cl-store:restore #p"~/Documents/trade_bot.db")
    (setf *nonce* nonce
          *history* history)))
