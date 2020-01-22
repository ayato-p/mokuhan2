(ns org.panchromatic.mokuhan2.ast2
  (:refer-clojure :exclude [newline partial])
  (:require [clojure.string :as str]
            [org.panchromatic.mokuhan2.util.string :as ustr]))

(def tags
  #{::variable-tag ::unescaped-variable-tag
    ::section-open-tag ::section-close-tag
    ::inverted-section-open-tag ::inverted-section-close-tag
    ::set-delimiter ::partial})

(declare node->mustache-str)

(defn syntax-tree
  ([]
   (syntax-tree []))
  ([nodes]
   {:type ::syntax-tree
    :nodes nodes}))

(defn syntax-tree->mustache-str
  [{:keys [nodes] :as st}]
  (str
   (reduce (fn [sb node]
             (ustr/append sb (node->mustache-str node)))
           (ustr/string-builder (count nodes))
           nodes)))

(defn standalone? [node]
  (get-in node [:tc :standalone?]))

(defn assoc-standalone [node standalone?]
  (assoc-in node [:tc :standalone?] standalone?))

(defn variable-tag [keys template-context]
  {:type ::variable-tag
   :ks keys
   :tc template-context})

(defn variable-tag->mustache-str
  [{:keys [ks tc] :as v-tag}]
  (str (get-in tc [:delimiters :open])
       (str/join "." ks)
       (get-in tc [:delimiters :close])))

(defn unescaped-variable-tag [keys template-context]
  {:type ::unescaped-variable-tag
   :ks keys
   :tc template-context})

(defn unescaped-variable-tag->mustache-str
  [{:keys [ks tc] :as un-v-tag}]
  (str (get-in tc [:delimiters :open]) "&"
       (str/join "." ks)
       (get-in tc [:delimiters :close])))

(defn section-open-tag [keys template-context]
  {:type ::section-open-tag
   :ks keys
   :tc template-context})

(defn section-open-tag->mustache-str
  [{:keys [ks tc] :as sec-o-tag}]
  (when sec-o-tag
    (str (get-in tc [:delimiters :open]) "#"
         (str/join "." ks)
         (get-in tc [:delimiters :close]))))

(defn section-close-tag [keys template-context]
  {:type ::section-close-tag
   :ks keys
   :tc template-context})

(defn section-close-tag->mustache-str
  [{:keys [ks tc] :as sec-c-tag}]
  (when sec-c-tag
    (str (get-in tc [:delimiters :open]) "/"
         (str/join "." ks)
         (get-in tc [:delimiters :close]))))

(defn section
  ([]
   (section nil nil))
  ([open-tag]
   (section open-tag nil []))
  ([open-tag close-tag]
   (section open-tag close-tag []))
  ([open-tag close-tag nodes]
   {:type ::section
    :closed? (some? close-tag)
    :nodes (cond-> []
             open-tag (conj open-tag)
             (seq nodes) (into nodes)
             close-tag (conj close-tag))}))

(defn section->mustache-str
  [{:keys [closed? nodes]}]
  (-> (section-open-tag->mustache-str (first nodes))
      (str (reduce (fn [sb node]
                     (ustr/append sb (node->mustache-str node)))
                   (ustr/string-builder (count nodes))
                   (when (seq nodes)
                     (subvec nodes 1 (dec (count nodes))))))
      (cond-> closed? (str (section-close-tag->mustache-str (last nodes))))))

(defn inverted-section-open-tag [keys template-context]
  {:type ::inverted-section-open-tag
   :ks keys
   :tc template-context})

(defn inverted-section-open-tag->mustache-str
  [{:keys [ks tc]}]
  (str (get-in tc [:delimiters :open]) "^"
       (str/join "." ks)
       (get-in tc [:delimiters :close])))

(defn inverted-section-close-tag [keys template-context]
  {:type ::inverted-section-close-tag
   :ks keys
   :tc template-context})

(defn inverted-section-close-tag->mustache-str
  [{:keys [ks tc] :as in-sec-c-tag}]
  (str (get-in tc [:delimiters :open]) "/"
       (str/join "." ks)
       (get-in tc [:delimiters :close])))

(defn inverted-section
  ([]
   (inverted-section nil nil))
  ([open-tag close-tag]
   (inverted-section open-tag close-tag []))
  ([open-tag close-tag nodes]
   {:type ::inverted-section
    :closed? (some? close-tag)
    :nodes (-> []
               (conj open-tag)
               (into nodes)
               (conj close-tag))}))

(defn inverted-section->mustache-str
  [{:keys [closed? nodes]}]
  (-> (inverted-section-open-tag->mustache-str (first nodes))
      (str (reduce (fn [sb node]
                     (ustr/append sb (node->mustache-str node)))
                   (ustr/string-builder (count nodes))
                   (when (seq nodes) (subvec nodes 1 (dec (count nodes))))))
      (cond-> closed? (str (inverted-section-close-tag->mustache-str (last nodes))))))

(defn partial [key template-context]
  {:type ::partial
   :k key
   :tc template-context})

(defn partial->mustache-str
  [{:keys [tc k]}]
  (str (get-in tc [:delimiters :open]) ">"
       k
       (get-in tc [:delimiters :close])))

(defn set-delimiter [delimiters template-context]
  {:type ::set-delimiter
   :delimiters delimiters
   :tc template-context})

(defn set-delimiter->mustache-str
  [{:keys [delimiters tc]}]
  (str (get-in tc [:delimiters :open])
       "="
       (get-in delimiters [:open])
       " "
       (get-in delimiters [:close])
       "="
       (get-in tc [:delimiters :close])))

(defn text [content template-context]
  {:type ::text
   :cont content
   :tc template-context})

(defn whitespace [content template-context]
  {:type ::whitespace
   :cont content
   :tc template-context})

(defn newline [content template-context]
  {:type ::newline
   :cont content
   :tc template-context})

(defn primitive->mustache-str
  [{:keys [cont]}]
  cont)

(defn- node->mustache-str
  [{:keys [type] :as node}]
  (let [f (case type
            ::variable-tag variable-tag->mustache-str
            ::unescaped-variable-tag unescaped-variable-tag->mustache-str
            ::section section->mustache-str
            ::inverted-section inverted-section->mustache-str
            ::partial partial->mustache-str
            ::set-delimiter set-delimiter->mustache-str
            (::text ::whitespace ::newline) primitive->mustache-str)]
    (f node)))
