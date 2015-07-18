(ns stock-algorithms.rsi
  "Implentation of the Relative Strength Indicator"
  (:require [stock-algorithms.sma :refer [sma]]))

(defn- rsi-ups-downs
  "Calculate the momentum (basically)"
  [period closes]
  (reduce (fn [[ups downs last-close] today-close]
            (cond (> today-close last-close)
                  [(conj ups (- today-close last-close)) downs today-close]
                  (> last-close today-close)
                  [ups (conj downs (- last-close today-close)) today-close]))
          [[] [] (first closes)]
          (take period (rest closes))))

(defn rsi
  "Calculate the rsi of the given period.
  Closes should have (inc period) elements and be sorted in ascending temporal order"
  [period closes]
  (let [[ups downs _] (rsi-ups-downs period closes)
        up-sma (sma ups)
        down-sma (sma downs)]
    (- 100 (/ 100 (+ 1 (/ up-sma down-sma))))))
