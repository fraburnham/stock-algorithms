(ns stock-algorithms.psar
  (:require [incanter.charts :as c]))

;;*
;;
;;*
(def alpha-base 0.02)

;;*
;;
;;*
(def alpha-inc 0.02)

;;*
;;
;;*
(def alpha-max 0.20)

;look for a cleaner way to handle the different alpha data globals are
;undesireable.
;high [last-high this-high]
;low [last-low this-low]
;close [last-close this-close]
(defn psar [sar alpha ep high low close]
  (let [[lh th] high
        [ll tl] low
        [_ tc] close
        uptrend (< sar tc)
        downtrend (> sar tc)
        [ep alpha] ((fn [[ep a]] ;make sure alpha doesn't exceed the max
                      [ep (if (> a alpha-max) alpha-max a)])
                     (cond (and ;new high, move ep
                             uptrend
                             (> th ep)) [th (+ alpha-inc alpha)]
                           (and ;new low, move ep
                             downtrend
                             (< tl ep)) [tl (+ alpha-inc alpha)]
                           :else [ep alpha]))
        n-sar (+ sar (* alpha (- ep sar)))]
    (cond
      ;trend reversal, adjust
      (and uptrend (> sar tl)) [ep alpha-base th]
      (and downtrend (< sar th)) [ep alpha-base tl]
      ;n-sar is inside or beyond price range, adjust
      (and uptrend (or (> n-sar tl) (> n-sar ll))) [(min ll tl) alpha ep]
      (and downtrend (or (< n-sar th) (< n-sar lh))) [(min lh th) alpha ep]
      :else [n-sar alpha ep])))

;;
;* x-data will be the data to use for the x axis and the sar-data will be used on the y axis
;;
(defn draw [chart x-data sar-data prices]
  ;psar is meaningless data without the price line
  (c/add-lines chart x-data prices)
  (c/add-points chart x-data sar-data))
