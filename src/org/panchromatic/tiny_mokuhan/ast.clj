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

(defrecord Variable [keys delimiters])

(defn variable [{:keys [keys delimiters] :as m}]
  (map->Variable m))

(defrecord UnescapedVariable [keys delimiters])

(defn unescaped-variable [{:keys [keys delimiters] :as m}]
  (map->UnescapedVariable m))
