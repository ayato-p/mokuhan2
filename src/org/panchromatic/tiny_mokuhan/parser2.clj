(ns org.panchromatic.tiny-mokuhan.parser2
  (:require [clojure.java.io :as io]
            [org.panchromatic.tiny-mokuhan.parser.internal :as internal]
            [org.panchromatic.tiny-mokuhan.reader :as reader]
            [org.panchromatic.tiny-mokuhan.util.misc :as umisc]
            [org.panchromatic.tiny-mokuhan.zip2 :as mzip]))

(def default-parse-options
  {:delimiters {:open "{{" :close "}}"}})

(defn parse [x options]
  (let [options (umisc/deep-merge default-parse-options options)
        open-delim (get-in options [:delimiters :open])
        buffer-size (inc (.length open-delim))]
    (with-open [reader (reader/pushback-reader (io/reader x) buffer-size)]
      (-> (internal/parse reader (internal/make-initial-state options))
          :ast
          mzip/complete))))
