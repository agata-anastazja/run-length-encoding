(ns run-length-encoding
  (:require [clojure.string :as str])
  )

(defn transform [acc x]

  (let [letterAsSymbol (symbol x)]
    (if (empty? acc)
    (conj acc {letterAsSymbol 1})
    (let [previousCharMap (last acc)
          previousCharSymbol (first (keys previousCharMap))
          sameChar? (= previousCharSymbol letterAsSymbol)]
      (if sameChar?
        (update (last acc) letterAsSymbol + 1)
        (conj acc {letterAsSymbol 1}))))))



(defn trans [elem]
  (let [occurences (count elem)
        firstElem (first elem)
        back-to-str (fn [x] (apply str x))]
    (if (> occurences 1)
      (str occurences firstElem)
      (back-to-str elem))))


(defn  run-length-encode
  "decodes a run-length-encoded string"
  [plain-text]
  (apply str (map trans (partition-by identity plain-text))))








(defn find-first-integer [cipher]
  (Integer/parseInt (re-find #"\d+" cipher) ) )

(defn split-at-first-int [cipher]
  (clojure.string/split cipher #"\d+" 2))

(defn first-char-after-int [cipher-split-at-int]
  (first (nth cipher-split-at-int 1)))

(defn multiply-first-char [char int]
  (take int (repeat char)))


(defn decode [cipher]
    (let [[head tail] (split-at-first-int cipher)]
    (if (= tail nil)
      cipher
      (let [first-int (find-first-integer cipher)
            multiplied-char (first tail)
            remaining (apply str (rest tail))]
        (apply str head (apply str (take first-int (repeat multiplied-char)))(if (empty? remaining) "" (decode remaining)))))))


(defn run-length-decode
  "encodes a string with run-length-encoding"
  [cipher-text]
  (decode cipher-text))


