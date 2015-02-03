(ns stock-algorithms.aroon
  (:require [incanter.charts :as c]))

(defn aroon-up [high-period periods]
  (* 100 (/ high-period periods)))

(defn aroon-down [low-period periods]
  (* 100 (/ low-period periods)))

;;
;* (count prices) is equal to the period
;;
(defn aroon [prices]
  (let [period (count prices)]
    ;spinning a loop to keep track of list index could use iterate...
    (loop [i 0
           high (first prices)
           up 0
           low (first prices)
           down 0
           prices prices]
      (if (empty? prices)
        [(aroon-up up period) (aroon-down down period)]
          ;compare highest and lowest in recur adjust if needed
          (recur (inc i)
                 (if (>= (first prices) high)
                   (first prices) high)
                 (if (>= (first prices) high)
                   i up)
                 (if (<= (first prices) low)
                   (first prices) low)
                 (if (<= (first prices) low)
                   i down)
                 (rest prices))))))

;;
;* Done in the same format(ish) as add-line so it can be used in a doto.
;* Should probably have some optional for the x arg so it can be used with date/time etc
;* or at an arbitrary point on an existing chart
;;
(defn draw [chart aroon-data period]
  (let [r (map (partial + period) (range (count aroon-data)))]
    (c/add-lines chart r (map (fn [[u d]] (- u d)) aroon-data))
    #_(c/add-lines chart r (map first aroon-data))
    #_(c/add-lines chart r (map second aroon-data))))
