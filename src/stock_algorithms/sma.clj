(ns stock-algorithms.sma
  "Calculate Simple Moving Average")

(def sum (partial reduce +))

(defn sma
  "Calculate the sma for a seq of prices"
  [prices]
  (/ (sum prices) (count prices)))
