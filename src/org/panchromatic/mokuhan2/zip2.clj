(ns org.panchromatic.mokuhan2.zip2
  (:refer-clojure :exclude [nil?])
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
   (-> (zip/zipper branch? children make-node root)
       zip/down)))

;; manipulator

(defn- nil? [loc]
  (clojure.core/nil? (zip/node loc)))

(defn append-node
  "Insert a node to right then moved to there."
  [loc node]
  (if (nil? loc)
    (zip/replace loc node)
    (-> (zip/insert-right loc node)
        zip/right)))

(defn- tag-node? [node]
  (contains? ast/tags (:type node)))

(defn look-behind [loc]
  (let [loc (loop [loc loc]
              (let [node (zip/node loc)]
                (cond
                  (tag-node? node)
                  (zip/edit loc ast/assoc-standalone false)
                  (and (= ::ast/section (:type node))
                       (:closed? node))
                  (-> (zip/down loc)
                      zip/rightmost
                      (zip/edit ast/assoc-standalone false)
                      zip/up)
                  :else
                  (recur (zip/left loc)))))]
    (zip/rightmost loc)))

(defn open-section [loc open-tag]
  (-> (append-node loc (ast/section open-tag))
      zip/down))

(defn open-inverted-section [loc open-tag]
  (-> (append-node loc (ast/inverted-section open-tag))
      zip/down))

(defn close-section [loc close-tag]
  (-> (append-node loc close-tag)
      zip/up
      (zip/edit assoc :closed? true)))

(defn complete [loc]
  (zip/root loc))
