;; # Knapsack
;; The [knapsack problem](https://en.wikipedia.org/wiki/Knapsack_problem) is an example of a problem that
;; we can use a greedy algorithm on.

;; ## explain problem informally
;; items have weights and values
;; you want to put items in knapsack so you end up with max value
;; we can apply the greedy algorithm to solve this problem.

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

;; ### decision variables
;; xi denotes item i is selected in the solution
;;
;; * xi = 1 item selected
;; * xi = 0 item NOT selected

;; ### problem constraints

;; the sum of the items weight can't be

;; $$
;; \sum_{i \in I} w\_{i} x\_{i} \leq kw
;; $$

;; ### objective function
;; capture the full value of knapsack

;; $$
;; \sum_{i \in I} v\_{i} x\_{i} \leq kw
;; $$

;; ### in summery


;; maximize
;; $$\sum_{i \in I} v\_{i} x\_{i}$$
;; subject to
;; $$
;; \sum_{i \in I} w\_{i} x\_{i} \leq K
;; $$
;; $$
;; x\_{i} \in\{0,1\} \quad(i \in I)
;; $$

;; ## dynamic programming
;; we can also use dynamic programming to solve the knapsack problem
;;

;;### key principles
;; To use dynamic programming
;; 
;; * divide and conquer
;; * bottom up computation,
;; * given a function that can solve for 
;; * bigger problem is made up of small sub problems of same nature


;; ### recurrence relation
;; dynamic programming used recurrence relations

;; 1. Assume you know how to solve every problem (picking items in this case)
;; up to some point. 
;; 2. then you just need to solve for the point before
;; 3. in this case, there are just two choices, to take the item or dont

;; lets demonstrate this really quick

;; first lets look at some example output. so scroll past these functions and
;; look at the example first, then come back to the code. Keep in mind this
;; isn't dynamic programming, were just  looking at the idea of a [recurrence relation](https://www2.cs.sfu.ca/~ggbaker/zju/math/recurrence.html)
;; which is when a term depends on one defined earlier.

(ns knapsack.core)

(defn sum-v
  [items]
  (->> items
       (map :v)
       (reduce +)))

(defn f
  [k si ri]
  (let [{:keys [w] :as i} (first ri)
        rr (rest ri)
        dont-take #(f k si rr)
        do-take #(f (- k w) (conj si i) rr)]
    (cond
     (nil? i) si
     (< k w) (dont-take) 
     :else (max-key sum-v (do-take) (dont-take))))) 

(let [items (for [w [10 2] v [1 2]] {:w w :v v})
      capacity 2
      saved-items []
      result (f capacity saved-items items)]
  {:result result :total-value (sum-v result)})

;; <pre><code>
;; => {:result [{:w 2, :v 2}], :total-value 2}
;; </code></pre>

;; this program however is fairly innecient because it recalculates the same
;; values several times. Lets time this and so we can justify trying something better
;; to do this were going to pull in a benchmarking library

(use 'criterium.core)

;; quoted out so it doesn't run
'(bench
  (let [items (for [w [10 2] v [1 2]] {:w w :v v})
        capacity 2
        saved-items []
        result (f capacity saved-items items)]
    {:result result :total-value (sum-v result)}))


;; output

;; * Evaluation count : 14215680 in 60 samples of 236928 calls.
;; * Execution time mean : 4.261307 µs
;; * Execution time std-deviation : 93.961846 ns
;; * Execution time lower quantile : 4.185448 µs ( 2.5%)
;; * Execution time upper quantile : 4.467629 µs (97.5%)
;; * Overhead used : 6.416068 ns

;; * Found 4 outliers in 60 samples (6.6667 %)
;; * low-severe   2 (3.3333 %)
;; * low-mild   2 (3.3333 %)
;; * Variance from outliers : 9.4644 % Variance is slightly inflated by outliers

;;  so mean tells the average times was in this unit
;;  $$\mu \mathrm{s}  $$
;; which is one millionth of a second, the std deviation was almost a billionth of
;; second. This isn't a surprise, our input was really small.

;; lets try our algorithm out on the problem sets provided by the class.
;; these sets contain inputs to our algorithm that we can then turn into 

(require '[clojure.java.io :as io])

(->> "knapsack/data/ks_4_0"
     io/resource
     slurp)
;; => "4 11\n8 4\n10 5\n15 8\n4 3\n"

;; the handout tells us the format is
;; n K
;; v_0 w_0
;; ...
;; V_n-1 w_n-1

;; the file contains n + 1 lines, where the first time tells us the
;; n: number of lines with data
;; K: capacity of the knapsack

;; already at this stage we can start to fret about how to best read in the
;; data. we can read it all at once into memory, we can consider processing them
;; one at a time.

;; In general, i would say there is no wrong answer to a question no one cares about.
;; In this case, lets aim for clarity to start, then will tune things if we can't
;; don't get a good enough grade. we need to make a function to convert our file
;;  into a format we can more easily use.

(use '[clojure.string :as str])

(let [[[n k] & items] (->> "knapsack/data/ks_4_0"
                           io/resource
                           slurp
                           str/split-lines
                           (mapv (fn [s] (mapv read-string (str/split s #" ")))))]
  
  {:n n :k k :items items})
;; => {:n 4, :k 11, :items ([8 4] [10 5] [15 8] [4 3])}

;; great that seems to work, now lets turn that into a function and add some
;; confidence

(def knap-dir "knapsack/data/")

(defn get-knapsack-input
  "size/n not sure what i is yet"
  [{:keys [size i] :or {i 0}}]
  (let [file (str knap-dir "/ks_" size "_" i)
        str->data (fn [s] (zipmap [:v :w]
                                 (mapv read-string (str/split s #" "))))
        [{:keys [v w]} & items] (->> file 
                                     io/resource
                                     slurp
                                     str/split-lines
                                     (mapv str->data))]
    
    {:n v :k w :ri (vec items)}))


(get-knapsack-input {:size 4})
;; => {:n 4, :k 11, :ri [{:v 8, :w 4} {:v 10, :w 5} {:v 15, :w 8} {:v 4, :w 3}]}

;;  so lets go ahead and try out some example sets!
;; hmm but what options do we have? lets make a function to quickly read the file
;; and give us an idea of the sizes.

(defn get-file-names
  [directory]
  (->> directory
       io/resource
       io/file
       file-seq
       rest
       (map (fn [f] (.getName f)))))
          
(get-file-names knap-dir)
;; => ("ks_lecture_dp_1" "ks_100_1" "ks_30_0" "ks_1000_0" "ks_60_0" "ks_40_0" "ks_100_0" "ks_100_2" "ks_4_0" "ks_50_0" "ks_lecture_dp_2" "ks_50_1" "ks_45_0" "ks_200_1" "ks_82_0" "ks_400_0" "ks_500_0" "ks_200_0" "ks_300_0" "ks_106_0" "ks_10000_0" "ks_19_0")
;; so that works but its a bit hard to read, it seems like the  only thing we care about is the size


(read-string (second (str/split "ks_100_1" #"_")))
;; => 100

;; lets go ahead and turn that into a function

(defn file-name->size
  [f]
  (read-string (second (str/split f #"_"))))

(defn get-input-sizes
  [directory]
  (->> directory
       get-file-names
       (map file-name->size)
       (filter int?)
       sort))

(get-input-sizes knap-dir)
;; => (4 19 30 40 45 50 50 60 82 100 100 100 106 200 200 300 400 500 1000 10000)

;; so 19 should work
(get-knapsack-input {:size 19})
;; => {:n 19, :k 31181, :i [{:v 1945, :w 4990} {:v 321, :w 1142}... 

;; great! lets go ahead and try our algorithm on size 19
;; first lets make the names a tad better and give saved-items (si) a default
;; initial value.


(defn fill
  ([k ri] (fill k ri []))
  ([k ri si]
   (let [{:keys [w] :as ci} (first ri)
         rr (rest ri)
         dont-take #(fill k rr si)
         do-take #(fill (- k w) rr (conj si ci))]
     (cond
       (nil? ci) si
       (< k w) (dont-take) 
       :else (max-key sum-v (do-take) (dont-take))))))

;; now we can use `fill` along with our helper functions to quickly find
;; the solution!

(->> {:size 4}
     get-knapsack-input
     ((juxt :k :ri))
     (apply fill))
;; => [{:v 15, :w 8} {:v 4, :w 3}]

;; fantastic, lets wrap that into another helpful function. i'm keeping names
;; minimal here to its easy for me to see everything going on. If I were to
;; make something public, the names would need to become more clear and we
;; would need documentation.

(->> {:size 4}
     get-knapsack-input
     ((juxt :k :ri)))


(defn kf->k
  [s]
  (->> {:size s}
       get-knapsack-input
       ((juxt :k :ri))
       (apply fill)))

(kf->k 19)
;; => [{:v 2945, :w 7390} {:v 1022, :w 2744} {:v 2890, :w 7280} {:v 1513, :w 3926} {:v 3878, :w 9656}]

(-> 19
    kf->k
    sum-v)
;; => 12248

(defn kf->v
  [s]
  (-> s
      kf->k
      sum-v))

'(kf->v 50)
;; this takes a long time to finish. Which is why its quoted here (it wont run), it doesn't in fact finish

;; looks like were going to need to try several kinds of functions. Lets open
;; our file->data functions to accept a fill function.

(defn kf->k2
  [{:keys [s f]}]
  (->> {:size s}
       get-knapsack-input
       ((juxt :k :ri))
       (apply f)))

(defn kf->v2
  [m]
  (-> m
      kf->k2
      sum-v))

;; lets save the knapsack sizes so we can reference them without running the code again

(def knapsack-sizes (get-input-sizes knap-dir))
;; => #'knapsack.core/knapsack-sizes

;; ok so we need a knapsack algorithm, 
(let [items [[0 0] [5 4] [6 5] [3 2]]
      capacity 9]
  (reduce
   (fn [k [c [v w]]]
     (let [prev-col (- (count k) (inc capacity))
           dont-take-v (max (or (last k) 0) (nth k prev-col 0))
           do-take-v (+ v (nth k (- prev-col w) 0))]
       (cond
         (zero? c) (conj k 0)
         (< c w) (conj k dont-take-v) 
         :else (conj k (max do-take-v dont-take-v)))))
   []
   (for [item items 
         c (range (inc capacity))]
     [c item])))

;; great this works, and follows the bottom up pattern were introduced
;; to in the lecture.

;; lets wrap

