(ns stock-algorithms.true-range)

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
