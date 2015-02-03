(ns stock-algorithms.obv)

(defn obv [last-obv volume close last-close]
  (cond (> close last-close) (+ last-obv volume)
        (< close last-close) (- last-obv volume)
        :else last-obv))
