(ns evolution.core
  (:gen-class))

;; This code defines and runs a genetic programming system on the problem
;; of finding a function that fits a particular set of [x y] pairs.

;; The aim here is mostly to demonstrate how genetic programming can be 
;; implemented in Clojure simply and clearly, and several things are 
;; done in somewhat inefficient and/or non-standard ways. But this should 
;; provide a reasonable starting point for developing more efficient/
;; standard/capable systems. 

;; Note also that this code, as written, will not always find a solution.
;; There are a variety of changes that one might make to improve its
;; problem-solving performance on the given problem. 

;; We'll use data from x^2 + x + 1 (the problem from chapter 4 of
;; http://www.gp-field-guide.org.uk/, although our gp algorithm won't
;; be the same, and we'll use some different parameters as well).

;; We'll use input (x) values ranging from -1.0 to 1.0 in increments
;; of 0.1, and we'll generate the target [x y] pairs algorithmically.
;; If you want to evolve a function to fit your own data then you could    
;; just paste a vector of pairs into the definition of target-data instead. 

(def target-data
  (map #(vector % (+ (* % %) % 1))
       (range -1.0 1.0 0.1)))

;; An individual will be an expression made of functions +, -, *, and
;; pd (protected division), along with terminals x and randomly chosen
;; constants between -5.0 and 5.0. Note that for this problem the 
;; presence of the constants actually makes it much harder, but that
;; may not be the case for other problems.

(def function-set 
  '(+
    -
    *
    pd))

(defn random-function 
  []
  (rand-nth function-set))

(defn variable-set
  [num-var]
  (def letters (map str (map char (range 97 123))))
  (def alphabet-size (count letters))

  (defn nth-var [n]
    (let [prefix "evo-"
          string-val (Integer/toString n alphabet-size)]
      (let [lookup-letters (seq string-val)]
        (let [indices (map (fn [letter] (Integer/parseInt (str letter) alphabet-size)) lookup-letters)]
          (clojure.string/join (concat (list prefix) (map (fn [i] (nth letters i)) indices)))))))

   (map (fn [i] (nth-var i)) (range 0 num-var)))

(defn random-terminal
  []
  (rand-nth (list 'x (- (rand 10) 5))))

(defn random-code
  [depth]
  (if (or (zero? depth)
          (zero? (rand-int 2)))
    (random-terminal)
    (list (random-function)
          (random-code (dec depth))
          (random-code (dec depth)))))

;; And we have to define pd (protected division):

(defn pd
  "Protected division; returns 0 if the denominator is zero."
  [num denom]
  (if (zero? denom)
    0
    (/ num denom)))

;; We can now evaluate the error of an individual by creating a function
;; built around the individual, calling it on all of the x values, and 
;; adding up all of the differences between the results and the 
;; corresponding y values.

(defn error 
  [individual]
  (let [value-function (eval (list 'fn '[x] individual))]
    (reduce + (map (fn [[x y]] 
                     (Math/abs 
                       (- (value-function x) y)))
                   target-data))))

;; We can now generate and evaluate random small programs, as with:

;; (let [i (random-code 3)] (println (error i) "from individual" i))

;; To help write mutation and crossover functions we'll write a utility
;; function that injects something into an expression and another that
;; extracts something from an expression.

(defn codesize [c]
  (if (seq? c)
    (count (flatten c))
    1))

(defn inject
  "Returns a copy of individual i with new inserted randomly somwhere within it (replacing something else)."
  [new i]
  (if (seq? i)
    (if (zero? (rand-int (count (flatten i))))
      new
      (if (< (rand)
              (/ (codesize (nth i 1))
                 (- (codesize i) 1)))
        (list (nth i 0) (inject new (nth i 1)) (nth i 2))
        (list (nth i 0) (nth i 1) (inject new (nth i 2)))))
    new))

(defn extract
  "Returns a random subexpression of individual i."
  [i]
  (if (seq? i)
    (if (zero? (rand-int (count (flatten i))))
      i
      (if (< (rand) (/ (codesize (nth i 1))
                       (- (codesize i)) 1))
        (extract (nth i 1))
        (extract (nth i 2))))
    i))

;; Now the mutate and crossover functions are easy to write:

(defn mutate
  [i]
  (inject (random-code 2) i))

(defn crossover
  [i j]
  (inject (extract j) i))

;; We can see some mutations with:
;; (let [i (random-code 2)] (println (mutate i) "from individual" i))

;; and crossovers with:
;; (let [i (random-code 2) j (random-code 2)]
;;   (println (crossover i j) "from" i "and" j))

;; We'll also want a way to sort a populaty by error that doesn't require 
;; lots of error re-computation:

(defn sort-by-error
  [population]
  (vec (map second
            (sort (fn [[err1 ind1] [err2 ind2]] (< err1 err2))
                  (map #(vector (error %) %) population)))))

;; Finally, we'll define a function to select an individual from a sorted 
;; population using tournaments of a given size.

(defn select
  [population tournament-size]
  (let [size (count population)]
    (nth population
         (apply min (repeatedly tournament-size #(rand-int size))))))

;; Now we can evolve a solution by starting with a random population and 
;; repeatedly sorting, checking for a solution, and producing a new 
;; population.

(defn evolve
  [popsize]
  (println "Starting evolution...")
  (loop [generation 0
         population (sort-by-error (repeatedly popsize #(random-code 2)))]
    (let [best (first population)
          best-error (error best)]
      (println "======================")
      (println "Generation:" generation)
      (println "Best error:" best-error)
      (println "Best program:" best)
      (println "     Median error:" (error (nth population 
                                                (int (/ popsize 2)))))
      (println "     Average program size:" 
               (float (/ (reduce + (map count (map flatten population)))
                         (count population))))
      (if (< best-error 0.1) ;; good enough to count as success
        (println "Success:" best)
        (recur 
          (inc generation)
          (sort-by-error      
            (concat
              (repeatedly (* 1/2 popsize) #(mutate (select population 7)))
              (repeatedly (* 1/4 popsize) #(crossover (select population 7)
                                                      (select population 7)))
              (repeatedly (* 1/4 popsize) #(select population 7)))))))))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
