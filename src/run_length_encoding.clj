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
  [cipher-text]
  (apply str (map trans (partition-by identity cipher-text))))





