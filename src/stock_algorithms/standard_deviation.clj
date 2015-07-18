(ns stock-algorithms.standard-deviation)

(defn standard-deviation [prices]
  (let [mean (/ (reduce + prices) (count prices))
        sq-diff (map #(Math/pow (- %1 mean) 2) prices)]
    (Math/sqrt (/ (reduce + sq-diff) (count sq-diff)))))
