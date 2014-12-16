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
  (with-precision 20
    (+ (* (- price prev-ema) (/ 2 (+ 1 period))) prev-ema)))

;*
; @return 1 for buy -1 for sell 0 for hold
;*
(defn ema-signal
  ([ema price] (ema-signal ema price 0))
  ([ema price envelope]
    (let [env (* ema envelope)
          upper (+ ema env)
          lower (- ema env)]
      (cond
        (> price upper) 1
        (< price lower) -1
        :else 0))))
