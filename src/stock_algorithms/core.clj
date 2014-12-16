(ns stock-algorithms.core)

;*
; Calculate the current ema for a period given the previous ema and the
; current price. EMA requires a warmup; if your period is 10 warm up with the
; first 10 datapoints. On the first call prev-ema is the same as price.
; @param period Number of time periods to average
; @param prev-ema The last result of ema (use price if this is the first call)
; @param price The current price
; @return The current ema
;*
(defn ema [period prev-ema price]
  (+ (* (- price prev-ema) (/ 2 (+ 1 period))) prev-ema))
