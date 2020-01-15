(ns org.panchromatic.mokuhan2.zip2
  (:require [clojure.zip :as zip]
            [org.panchromatic.mokuhan2.ast2 :as ast]))

(def ^:private branch-node
  #{::ast/syntax-tree ::ast/section ::ast/inverted-section})

(defn- branch? [node]
  (contains? branch-node (:type node)))

(defn- children [node]
  (get node :nodes))

(defn- make-node [node nodes]
  (assoc node :nodes nodes))

(defn ast-zip
  ([]
   (ast-zip (ast/syntax-tree)))
  ([root]
   (zip/zipper branch?
               children
               make-node
               root)))

(defn append-primitive [loc primitive]
  (zip/append-child loc primitive))

(defn append-tag [loc tag]
  (loop [loc (-> loc
                 (zip/append-child tag)
                 zip/down
                 zip/rightmost)]
    (let [left-node (some-> loc zip/left zip/node)]
      (if (or (nil? left-node)
              (= ::ast/newline (:type left-node))
              (false? (ast/standalone? left-node)))
        (zip/up loc)
        (recur (zip/edit (zip/left loc) ast/update-standalone false))))))

(defn append&into-section [loc]
  (-> loc
      (zip/append-child (ast/section))
      zip/down
      zip/rightmost))

(defn out-section [loc]
  (zip/up loc))

(defn assoc-open-section-tag [loc tag]
  (zip/edit loc ast/assoc-open-section-tag tag))

(defn assoc-close-section-tag [loc tag]
  (zip/edit loc ast/assoc-close-section-tag tag))

(defn complete [loc]
  (zip/root loc))
