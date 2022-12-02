(ns day-2
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def opponent-throws
  {"A" :rock
   "B" :paper
   "C" :scissor})

(def player-throws
  {"X" :rock
   "Y" :paper
   "Z" :scissor})

(def throw-scores
  {:rock    1
   :paper   2
   :scissor 3})

(def loss?
  (into #{} (comp (partition-all 2) (take 6)) (cycle [:scissor :paper :rock])))

(def win?
  (into #{} (comp (partition-all 2) (take 6)) (cycle [:rock :paper :scissor])))

(defn- calculate-result
  [opponent-throw player-throw]
  (cond
    (= opponent-throw player-throw)       :draw
    (win? [opponent-throw player-throw])  :win
    (loss? [opponent-throw player-throw]) :loss))

(def result-scores
  {:draw 3
   :win  6
   :loss 0})

(defn- part-one-helper
  [round]
  (let [[opponent player] (str/split round #"\s")
        opponent-throw    (get opponent-throws opponent)
        player-throw      (get player-throws player)
        result            (calculate-result opponent-throw player-throw)]
    (+ (get throw-scores player-throw)
       (get result-scores result))))

(defn part-one
  []
  (with-open [rdr (io/reader (io/resource "./input.txt"))]
    (let [input (line-seq rdr)]
      (transduce (map part-one-helper) + 0 input))))

(def player-results
  {"X" :loss
   "Y" :draw
   "Z" :win})

(defn- lookup-throw
  [opponent-throw]
  (fn [[opp-throw player-throw]]
    (when (= opponent-throw opp-throw)
      player-throw)))

(defn- calculate-throw
  [opponent-throw player-result]
  (case player-result
    :draw opponent-throw
    :win  (some (lookup-throw opponent-throw) win?)
    :loss (some (lookup-throw opponent-throw) loss?)))

(defn- part-two-helper
  [round]
  (let [[opponent player] (str/split round #"\s")
        opponent-throw    (get opponent-throws opponent)
        player-result     (get player-results player)
        player-throw      (calculate-throw opponent-throw player-result)]
    (+ (get throw-scores player-throw)
       (get result-scores player-result))))

(defn part-two
  []
  (with-open [rdr (io/reader (io/resource "./input.txt"))]
    (let [input (line-seq rdr)]
      (transduce (map part-two-helper) + 0 input))))
