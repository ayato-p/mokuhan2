(ns org.panchromatic.mokuhan2.renderer
  (:require [clojure.string :as str])
  (:import [org.panchromatic.tiny_mokuhan.ast Mustache NewLine Text Variable]))

(defprotocol Renderer
  (-render [renderable data]))

(extend-type Mustache
  Renderer
  (-render [mustache data]
    (.toString
     (reduce (fn [sb c] (.append sb (-render c data)))
             (StringBuilder.)
             (.contents mustache)))))

(extend-type Text
  Renderer
  (-render [text _]
    (.content text)))

(extend-type NewLine
  Renderer
  (-render [_ _]
    (System/lineSeparator)))

(defn- escape-html [s]
  (-> s
      (str/replace #"&" "&amp;")
      (str/replace #"<" "&lt;")
      (str/replace #">" "&gt;")
      (str/replace #"\"" "&quot;")
      (str/replace #"'" "&#39;")))

(extend-type Variable
  Renderer
  (-render [variable data]
    (escape-html (get data (keyword (.keys variable))))))

(defn render [ast data]
  (-render ast data))
