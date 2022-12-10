(ns day-10
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn- parse-instruction
  [line]
  (let [[instruction operand] (str/split line #"\s")]
    (cond-> [(keyword instruction)]
      (some? operand)
      (conj (parse-long operand)))))

(defn- execute-instruction
  [{:keys [time x]} [op-code operand]]
  (case op-code
    :noop
    [{:time (inc time)
      :x    x}]

    :addx
    [{:time (inc time)
      :x    x}
     {:time (+ 2 time)
      :x    (+ x operand)}]

    []))

(defn- execute-instructions
  [environment instructions]
  (loop [instructions instructions
         environments [environment]]
    (if (seq instructions)
      (recur
       (rest instructions)
       (into
        environments
        (execute-instruction
         (peek environments)
         (first instructions))))
      environments)))

(defn- signal-strength
  [{:keys [time x]}]
  (* time x))

(defn part-one
  []
  (with-open [rdr (io/reader (io/resource "./input.txt"))]
    (let [input        (line-seq rdr)
          instructions (map parse-instruction input)
          xf           (comp (drop 19) (take-nth 40) (map signal-strength))]
      (transduce xf + 0 (execute-instructions {:time 1 :x 1} instructions)))))

(defn- pixel
  [{:keys [time x]}]
  (let [adjusted-time (mod time 40)
        draw?         (into #{} (map #(- adjusted-time %) (range 0 3)))]
    (if (draw? x) \# \.)))

(defn part-two
  []
  (with-open [rdr (io/reader (io/resource "./input.txt"))]
    (let [input        (line-seq rdr)
          instructions (map parse-instruction input)]
      (run!
       println
       (eduction
        (map pixel)
        (partition-all 40)
        (map str/join)
        (execute-instructions {:time 1 :x 1} instructions))))))
