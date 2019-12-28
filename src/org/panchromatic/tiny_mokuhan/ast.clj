(ns org.panchromatic.tiny-mokuhan.ast
  (:refer-clojure :exclude [newline])
  (:require [clojure.zip :as zip]))

(defrecord Mustache [contents])

(defn mustache
  ([]
   (mustache []))
  ([contents]
   (Mustache. contents)))

(defrecord Text [content])

(defn text [content]
  (Text. content))

(defrecord NewLine [])

(defn newline []
  (NewLine.))

(defrecord Variable [keys delimiters])

(defn variable [keys delimiters]
  (Variable. keys delimiters))

(defrecord UnescapedVariable [keys delimiters])

(defn unescaped-variable [keys delimiters]
  (UnescapedVariable. keys delimiters))
