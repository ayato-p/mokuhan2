(ns org.panchromatic.tiny-mokuhan.ast
  (:require [clojure.zip :as zip]))

(defrecord Mustache [contents])

(defrecord Text [content])

(defrecord Variable [id open-delim close-delim])

(defrecord NewLine [])
