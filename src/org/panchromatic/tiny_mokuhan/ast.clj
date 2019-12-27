(ns org.panchromatic.tiny-mokuhan.ast
  (:refer-clojure :exclude [newline])
  (:require [clojure.zip :as zip]))

(defrecord Mustache [contents])

(defn mustache
  ([]
   (mustache {:contents []}))
  ([{:keys [contents] :as m}]
   (map->Mustache m)))

(defrecord Text [content])

(defn text [{:keys [content] :as m}]
  (map->Text m))

(defrecord NewLine [])

(defn newline []
  (->NewLine))

(defrecord Variable [keys open-delim close-delim])

(defn variable
  [{:keys [keys open-delim close-delim]
    :as m}]
  (map->Variable m))

(defrecord UnescapedVariable [keys open-delim close-delim])

(defn unescaped-variable
  [{:keys [keys open-delim close-delim]
    :as m}]
  (map->UnescapedVariable m))
