(ns day-7
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   (java.nio.file Paths)))

(defn- join-paths
  [base-path sub-directory]
  (if (nil? base-path)
    (Paths/get sub-directory (into-array String []))
    (.. base-path (resolve sub-directory) normalize)))

(defn- path->ks
  [path]
  (let [segments (-> path str (str/split #"\/"))]
    (->> segments
         (remove str/blank?)
         (mapv keyword))))

(defn- construct-world
  [{:keys [pwd ks] :as acc} line]
  (cond
    (str/starts-with? line "$ cd ")
    (let [directory             (subs line 5)
          new-working-directory (join-paths pwd directory)]
      (-> acc
          (assoc :pwd new-working-directory)
          (assoc :ks (into [:root] (path->ks new-working-directory)))))

    (str/starts-with? line "$ ls")
    acc
    
    :else
    (let [[size filename] (str/split line #"\s")]
      (if-not (= "dir" size)
        (update acc :tree update ks assoc filename (parse-long size))
        (update acc :tree update ks assoc filename -1)))))

(defn- directory-size
  [tree ks]
  (->> (get tree ks)
       (mapcat
        (fn [[file size]]
          (if (neg? size)
            (directory-size tree (conj ks (keyword file)))
            [size])))))

(defn part-one
  []
  (with-open [rdr (io/reader (io/resource "./input.txt"))]
    (let [input (line-seq rdr)
          world (reduce construct-world {} input)]
      (transduce
       (comp
        (map
         (fn [[ks _dir]]
           [ks (reduce + 0 (directory-size (:tree world) ks))]))
        (filter
         (fn [[_ks size]]
           (>= 100000 size)))
        (map second))
       +
       0
       (:tree world)))))

(def needed   30000000)
(def max-size 70000000)

(defn part-two
  []
  (with-open [rdr (io/reader (io/resource "./input.txt"))]
    (let [input         (line-seq rdr)
          world         (reduce construct-world {} input)
          root-size     (reduce + 0 (directory-size (:tree world) [:root]))
          unused-size   (- max-size root-size)
          required-size (- needed unused-size)]
      (->> (:tree world)
           (map
            (fn [[ks _dir]]
              [ks (reduce + 0 (directory-size (:tree world) ks))]))
           (filter
            (fn [[_ks size]]
              (<= required-size size)))
           (apply min-key second)
           second))))
