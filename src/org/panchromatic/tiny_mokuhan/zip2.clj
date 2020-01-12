(ns org.panchromatic.tiny-mokuhan.zip2
  (:require [clojure.zip :as zip]
            [org.panchromatic.tiny-mokuhan.ast2 :as ast]))

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

(defn complete [loc]
  (zip/root loc))
