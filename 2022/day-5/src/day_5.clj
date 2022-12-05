(ns day-5
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def parse-input-xf
  (comp 
   (partition-by empty?)
   (remove #(every? empty? %))))

(defn- parse-board
  [board]
  (let [ids        (-> board last (str/split #"\s+") rest)
        components (->> board butlast)]
    (into
     {}
     (for [id ids]
       [(parse-long id)
        (->> (for [component components]
               (get component (inc (* 4 (dec (parse-long id))))))
             (remove #{\space})
             reverse)]))))

(defn- parse-instruction
  [instruction]
  (->> instruction
       (re-seq #"move (\d+) from (\d+) to (\d+)")
       first
       rest
       (map parse-long)))

(defn- move-crates-one
  [board instructions]
  (reduce
   (fn [new-board [number source destination]]
     (loop [inner-board new-board
            number      number]
       (if (zero? number)
         inner-board
         (recur
          (let [target (last (get inner-board source))]
            (-> inner-board
                (update source butlast)
                (update destination #(concat % [target]))))
          (dec number)))))
   board
   instructions))

(defn part-one
  []
  (with-open [rdr (io/reader (io/resource "./input.txt"))]
    (let [input                (line-seq rdr)
          [board instructions] (eduction parse-input-xf input)
          board                (parse-board board)
          instructions         (map parse-instruction instructions)
          final-board          (move-crates-one board instructions)]
      (str/join
       ""
       (for [num (range (count final-board))]
         (last (get final-board (inc num))))))))

(defn- move-crates-two
  [board instructions]
  (reduce
   (fn [new-board [number source destination]]
     (let [target (take-last number (get new-board source))]
       (-> new-board
           (update source #(drop-last number %))
           (update destination concat target))))
   board
   instructions))

(defn part-two
  []
  (with-open [rdr (io/reader (io/resource "./input.txt"))]
    (let [input                (line-seq rdr)
          [board instructions] (eduction parse-input-xf input)
          board                (parse-board board)
          instructions         (map parse-instruction instructions)
          final-board          (move-crates-two board instructions)]
      (str/join
       ""
       (for [num (range (count final-board))]
         (last (get final-board (inc num))))))))
