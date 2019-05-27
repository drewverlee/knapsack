(ns knapsack.core)

(defn -main
  [& args]
  (println "solution"))

;; # Knapsack
;; so the knapsack problem is an example of a problem that
;; we can use a greedy algorithm

;; ## explain problem
;; items have weights and values
;; you want to put items in knapsack so you end up with max value

;; ## greedy algorithm
;; pick a formula/heuristic and follow it tell you run out
;; e.g sort by value/weight and pick tell full


;; ## formal description of knapsack
;; weight: w
;; value: v
;; knapsack capacity: kc
;; knapsack weight : kw
;; knapsack value : kv
;; 
;; find subset of i in I
;; * kv is max for all KV
;; * kw <= kc

;; xi denotes weither item i is selected in the solution
;; xi = 1 item selected
;; xi = 0 item NOT selected

;; ## constraints
;; 
