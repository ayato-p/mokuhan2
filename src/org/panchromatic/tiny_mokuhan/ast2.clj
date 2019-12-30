(ns org.panchromatic.tiny-mokuhan.ast2
  (:refer-clojure :exclude [newline partial])
  (:require [clojure.string :as str]))

(declare node->mustache-str)

(defn syntax-tree
  ([]
   (syntax-tree []))
  ([nodes]
   {::type ::syntax-tree
    ::nodes nodes}))

(defn syntax-tree->mustache-str
  [{::keys [nodes] :as st}]
  (.toString
   (reduce (fn [^StringBuilder sb node]
             (.append sb (node->mustache-str node)))
           (StringBuilder. (count nodes))
           nodes)))

(defn template-context
  [{:keys [open close] :as delimiters} row-num col-num standalone?]
  {::type ::template-context
   ::delims delimiters
   ::row row-num
   ::column col-num
   ::standalone? standalone?})

(defn variable-tag [keys template-context]
  {::type ::variable-tag
   ::ks keys
   ::tc template-context})

(defn variable-tag->mustache-str
  [{::keys [ks tc] :as v-tag}]
  (str (get-in tc [::delims :open])
       (str/join "." ks)
       (get-in tc [::delims :close])))

(defn unescaped-variable-tag [keys template-context]
  {::type ::variable-tag
   ::ks keys
   ::tc template-context})

(defn unescaped-variable-tag->mustache-str
  [{::keys [ks tc] :as un-v-tag}]
  (str (get-in tc [::delims :open]) "&"
       (str/join "." ks)
       (get-in tc [::delims :close])))

(defn open-section-tag [keys template-context]
  {::type ::open-section-tag
   ::ks keys
   ::tc template-context})

(defn open-section-tag->mustache-str
  [{::keys [ks tc] :as o-sec-tag}]
  (str (get-in tc [::delims :open]) "#"
       (str/join "." ks)
       (get-in tc [::delims :close])))

(defn close-section-tag [keys template-context]
  {::type ::close-section-tag
   ::ks keys
   ::tc template-context})

(defn close-section-tag->mustache-str
  [{::keys [ks tc] :as c-sec-tag}]
  (str (get-in tc [::delims :open]) "/"
       (str/join "." ks)
       (get-in tc [::delims :close])))

(defn section
  ([]
   (section nil nil))
  ([open-tag close-tag]
   (section open-tag close-tag []))
  ([open-tag close-tag nodes]
   {::type ::section
    ::o-tag open-tag
    ::c-tag close-tag
    ::nodes nodes}))

(defn section->mustache-str
  [{::keys [o-tag c-tag nodes]}]
  (str (open-section-tag->mustache-str o-tag)
       (reduce (fn [^StringBuilder sb node]
                 (.append sb (node->mustache-str node)))
               (StringBuilder. (count nodes))
               nodes)
       (close-section-tag->mustache-str c-tag)))

(defn open-inverted-section-tag [keys template-context]
  {::type ::open-inverted-section-tag
   ::ks keys
   ::tc template-context})

(defn open-inverted-section-tag->mustache-str
  [{::keys [ks tc]}]
  (str (get-in tc [::delims :open]) "^"
       (str/join "." ks)
       (get-in tc [::delims :close])))

(defn close-inverted-section-tag [keys template-context]
  {::type ::close-inverted-section-tag
   ::ks keys
   ::tc template-context})

(defn close-inverted-section-tag->mustache-str
  [{::keys [ks tc] :as c-in-sec-tag}]
  (str (get-in tc [::delims :open]) "/"
       (str/join "." ks)
       (get-in tc [::delims :close])))

(defn inverted-section
  ([]
   (inverted-section nil nil))
  ([open-tag close-tag]
   (inverted-section open-tag close-tag []))
  ([open-tag close-tag nodes]
   {::type ::inverted-section
    ::o-tag open-tag
    ::c-tag close-tag
    ::nodes nodes}))

(defn inverted-section->mustache-str
  [{::keys [o-tag c-tag nodes]}]
  (str (open-inverted-section-tag->mustache-str o-tag)
       (reduce (fn [^StringBuilder sb node]
                 (.append sb (node->mustache-str node)))
               (StringBuilder. (count nodes))
               nodes)
       (close-inverted-section-tag->mustache-str c-tag)))

(defn partial [key template-context]
  {::type ::partial
   ::k key
   ::tc template-context})

(defn partial->mustache-str
  [{::keys [tc k]}]
  (str (get-in tc [::delims :open]) ">"
       k
       (get-in tc [::delims :close])))

(defn set-delimiter [delimiters template-context]
  {::type ::set-delimiter
   ::delims delimiters
   ::tc template-context})

(defn set-delimiter->mustache-str
  [{::keys [delims tc]}]
  (str (get-in tc [::delims :open])
       "="
       (get-in delims [:open])
       " "
       (get-in delims [:close])
       "="
       (get-in tc [::delims :close])))

(defn text [content template-context]
  {::type ::text
   ::cont content
   ::tc template-context})

(defn whitespace [content template-context]
  {::type ::whitespace
   ::cont content
   ::tc template-context})

(defn newline [content template-context]
  {::type ::newline
   ::cont content
   ::tc template-context})

(defn primitive->mustache-str
  [{::keys [cont]}]
  cont)

(defn- node->mustache-str
  [{::keys [type] :as node}]
  (let [f (case type
            ::variable-tag variable-tag->mustache-str
            ::section section->mustache-str
            ::inverted-section inverted-section->mustache-str
            ::partial partial->mustache-str
            ::set-delimiter set-delimiter->mustache-str
            (::text ::whitespace ::newline) primitive->mustache-str)]
    (f node)))