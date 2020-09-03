(ns run-length-encoding
  (:require [clojure.string :as str])
  )

(defn  run-length-encode
  "decodes a run-length-encoded string"
  [plain-text]
  (->>
    (partition-by identity plain-text)
    (mapcat (juxt count first))
    (remove #(= %1 \1))
    (apply str)))




(defn run-length-decode
  "encodes a string with run-length-encoding"
  [cipher]
  (->>
    cipher
    (re-seq #"(\d+)?(\D)")
    (mapcat (fn [x]
              (let [repetitions (nth x 1)
                    char (nth x 2)]
                (if repetitions
                  (repeat (Integer/parseInt repetitions) char )
                  char)
                   )))
    (apply str)))


