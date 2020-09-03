(ns run-length-encoding
  (:require [clojure.string :as str])
  )

(defn  run-length-encode
  "decodes a run-length-encoded string"
  [plain-text]
  (->>
    (partition-by identity plain-text)
    (mapcat (juxt count first))
    (remove #{1})
    (apply str)))




(defn run-length-decode
  "encodes a string with run-length-encoding"
  [cipher]
  (->>
    cipher
    (re-seq #"(\d+)?(\D)")
    (mapcat #(let [[_ repetitions char] %1]
                (repeat (Integer/parseInt (or repetitions "1")) char)))
    (apply str)))


