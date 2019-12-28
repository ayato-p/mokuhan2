(ns org.panchromatic.tiny-mokuhan.renderer
  (:import [org.panchromatic.tiny_mokuhan.ast Mustache Text NewLine Variable]))

(defprotocol Renderer
  (-render [renderable]))

(extend-type Mustache
  Renderer
  (-render [mustache]
    (.toString
     (reduce (fn [sb c] (.append sb (-render c)))
             (StringBuilder.)
             (.contents mustache)))))

(extend-type Text
  Renderer
  (-render [text]
    (.content text)))

(extend-type NewLine
  Renderer
  (-render [_]
    (System/lineSeparator)))

(defn render [ast]
  (-render ast))
