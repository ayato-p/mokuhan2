(ns org.panchromatic.mokuhan2.util.string
  (:require [clojure.string :as str]))

;; StringBuilder
(defn string-builder
  ([]
   (string-builder 10))

  ([n]
   (StringBuilder. n)))

(defn append [sb s]
  (.append sb s))

;; String
(defn length [s]
  (.length s))

(defn char-at [s idx]
  (.charAt s idx))
