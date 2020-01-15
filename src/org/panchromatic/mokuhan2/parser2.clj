(ns org.panchromatic.mokuhan2.parser2
  (:require [clojure.java.io :as io]
            [org.panchromatic.mokuhan2.parser.internal :as internal]
            [org.panchromatic.mokuhan2.reader :as reader]
            [org.panchromatic.mokuhan2.util.misc :as umisc]
            [org.panchromatic.mokuhan2.zip2 :as mzip]))

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
