(ns org.panchromatic.mokuhan2.zip2
  (:require [clojure.zip :as zip]
            [org.panchromatic.mokuhan2.ast2 :as ast]))

;; Zipper

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

;; manipulator

(defn append-node
  "Insert a primitive node then moved to there."
  [loc primitive]
  (if (zip/branch? loc)
    (-> (zip/insert-child loc primitive)
        zip/down)
    (-> (zip/insert-right loc primitive)
        zip/right)))

(def ^:private tag-node
  #{::ast/variable-tag ::ast/unescaped-variable-tag})

(defn- tag-node? [node]
  (contains? tag-node (:type node)))

(defn look-behind-for-not-standalone [loc]
  (let [current (zip/node loc)]
    (loop [loc loc]
      (let [left-node (some-> loc zip/left zip/node)]
        (if (or (nil? left-node)
                (= ::ast/newline (:type left-node))
                (false? (ast/standalone? left-node)))
          (zip/rightmost loc)
          (recur (cond-> (zip/left loc)
                   (tag-node? left-node)
                   (zip/edit ast/assoc-standalone false))))))))

(defn complete [loc]
  (zip/root loc))
