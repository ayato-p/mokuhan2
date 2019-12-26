(ns org.panchromatic.tiny-mokuhan.zip
  (:require [clojure.zip :as zip]
            [org.panchromatic.tiny-mokuhan.ast :as ast])
  (:import org.panchromatic.tiny_mokuhan.ast.Mustache))

(defprotocol ASTZipper
  (branch? [this])
  (children [this])
  (make-node [this children]))

(defn ast-zip [root]
  (zip/zipper branch?
              children
              make-node
              root))

(extend-protocol ASTZipper
  Object
  (branch? [_] false)

  Mustache
  (branch? [mustache]
    true)
  (children [mustache]
    (.contents mustache))
  (make-node [mustache contents]
    (ast/->Mustache contents)))

(defn append-children [loc items]
  (reduce #(zip/append-child %1 %2)
          loc
          items))
