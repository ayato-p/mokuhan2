(ns org.panchromatic.tiny-mokuhan.util.string
  (:require [clojure.string :as str]))

;; StringBuilder
(defn string-builder []
  (StringBuilder.))

(defn append [sb s]
  (.append sb s))

;; String
(defn length [s]
  (.length s))

(defn char-at [s idx]
  (.charAt s idx))
