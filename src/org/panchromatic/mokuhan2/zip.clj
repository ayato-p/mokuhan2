(ns org.panchromatic.mokuhan2.zip
  (:require [clojure.zip :as zip]
            [org.panchromatic.mokuhan2.ast :as ast])
  (:import org.panchromatic.mokuhan2.ast.Mustache))

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
