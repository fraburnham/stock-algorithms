(ns stock-algorithms.core)

;;*
;; Calculate the current ema for a period given the previous ema and the
;; current price. EMA requires a warmup; if your period is 10 warm up with the
;; first 10 datapoints. On the first call prev-ema is the same as price.
;; @param period Number of time periods to average
;; @param prev-ema The last result of ema (use price if this is the first call)
;; @param price The current price
;; @return The current ema
;;*
(defn ema [period prev-ema price]
  (with-precision 20
    (+ (* (- price prev-ema) (/ 2 (+ 1 period))) prev-ema)))

;;*
;; @return 1 for buy -1 for sell 0 for hold
;;*
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

;;*
;;
;;*
(defn true-range [high low last-close]
  (reduce #(Math/max %1 %2)
          [(- high low) (Math/abs (- high last-close))
           (Math/abs (- low last-close))]))

;;*
;; Calculate the first ATR value before using the standard ATR algo.
;; @param period The period to use for ATR. This must match the (count data)
;; @param data The data needed for warming up ATR. In the format
;;             ((high low last-close) (high low last-close))
;; @return The first ATR value
;;*
(defn atr-warmup [period data]
  (if (not (= (count data) period))
    nil
    (/ (reduce +
               (map (fn [[high low last-close]]
                      (true-range high low last-close))
                    data))
       period)))

;;*
;; Calculate average true range to determine price volatility.
;;*
(defn atr [high low last-close last-atr period]
  (let [tr (true-range high low last-close)]
    (/ (+ (* last-atr (- period 1)) tr) period)))


(defn standard-deviation [prices]
  (let [mean (/ (reduce + prices) (count prices))
        sq-diff (map #(Math/pow (- %1 mean) 2) prices)]
    (Math/sqrt (/ (reduce + sq-diff) (count sq-diff)))))

(defn rsi [period up-ema down-ema close last-close]
  (let [up (if (> close last-close)
             (- close last-close)
             0)
        down (if (> close last-close)
               0
               (- last-close close))
        n-up-ema (ema period up-ema up)
        n-down-ema (ema period down-ema down)
        rs (/ n-up-ema n-down-ema)]
    [(- 100 (/ 100 (+ 1 rs))) n-up-ema n-down-ema]))

(defn tsi
  ([close last-close r-ema s-ema abs-r-ema abs-s-ema]
    (tsi close last-close 25 13 r-ema s-ema abs-r-ema abs-s-ema))
  ;r is inner smoothing period for momentum
  ;s is outter smoothing period for momentum
  ([close last-close r s r-ema s-ema abs-r-ema abs-s-ema]
    (let [m (- close last-close)
          n-r-ema (ema r r-ema m)
          n-s-ema (ema s s-ema n-r-ema)
          n-abs-r-ema (ema r abs-r-ema (Math/abs m))
          n-abs-s-ema (ema s abs-s-ema n-abs-r-ema)]
      [(* 100 (/ n-s-ema n-abs-s-ema))
       n-r-ema n-s-ema n-abs-r-ema n-abs-s-ema])))
