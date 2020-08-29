(ns run-length-encoding
  (:require [clojure.string :as str])
  )

(defn transform [elem]
  (let [occurences (count elem)
        firstElem (first elem)
        back-to-str (fn [x] (apply str x))]
    (if (> occurences 1)
      (str occurences firstElem)
      (back-to-str elem))))


(defn  run-length-encode
  "decodes a run-length-encoded string"
  [plain-text]
  (apply str (map transform (partition-by identity plain-text))))


(defn find-first-integer [cipher]
  (Integer/parseInt (re-find #"\d+" cipher) ) )

(defn split-at-first-int [cipher]
  (clojure.string/split cipher #"\d+" 2))

(defn multiply-first-char [char int]
  (apply str (take int (repeat char))))


(defn run-length-decode
  "encodes a string with run-length-encoding"
  [cipher]
    (let [[head tail] (split-at-first-int cipher)]
      (if (= tail nil)
        cipher
        (let [first-int (find-first-integer cipher)
              multiplied-char (first tail)
              remaining (apply str (rest tail))]
          (apply str head (multiply-first-char multiplied-char first-int) (run-length-decode remaining))))))



