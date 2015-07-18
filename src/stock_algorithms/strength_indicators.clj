(ns stock-algorithms.strength-indicators
  (:require [stock-algorithms.ema :as e]))

(defn rsi [period up-ema down-ema close last-close]
  (let [up (if (> close last-close)
             (- close last-close)
             0)
        down (if (> close last-close)
               0
               (- last-close close))
        n-up-ema (e/ema period up-ema up)
        n-down-ema (e/ema period down-ema down)
        rs (/ n-up-ema n-down-ema)]
    [(- 100 (/ 100 (+ 1 rs))) n-up-ema n-down-ema]))

(defn tsi
  ([close last-close r-ema s-ema abs-r-ema abs-s-ema]
    (tsi close last-close 25 13 r-ema s-ema abs-r-ema abs-s-ema))
  ;r is inner smoothing period for momentum
  ;s is outter smoothing period for momentum
  ([close last-close r s r-ema s-ema abs-r-ema abs-s-ema]
    (let [m (- close last-close)
          n-r-ema (e/ema r r-ema m)
          n-s-ema (e/ema s s-ema n-r-ema)
          n-abs-r-ema (e/ema r abs-r-ema (.abs m))
          n-abs-s-ema (e/ema s abs-s-ema n-abs-r-ema)]
      [(* 100 (/ n-s-ema n-abs-s-ema))
       n-r-ema n-s-ema n-abs-r-ema n-abs-s-ema])))
