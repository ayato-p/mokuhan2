(ns org.panchromatic.tiny-mokuhan.zip2
  (:require [org.panchromatic.tiny-mokuhan.ast2 :as ast]
            [clojure.zip :as zip]))

(def ^:private branch-node
  #{::ast/syntax-tree ::ast/section ::ast/inverted-section})

(defn- branch? [node]
  (contains? branch-node (:type node)))

(defn- children [node]
  (get node :nodes))

(defn- make-node [node nodes]
  (assoc node :nodes nodes))

(defn ast-zip []
  (zip/zipper branch?
              children
              make-node
              (ast/syntax-tree)))

(defn append-primitive [loc primitive]
  (zip/append-child loc primitive))
