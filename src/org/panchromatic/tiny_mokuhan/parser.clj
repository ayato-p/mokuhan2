(ns org.panchromatic.tiny-mokuhan.parser
  (:require [clojure.zip :as zip]
            [org.panchromatic.tiny-mokuhan.ast :as ast]
            [org.panchromatic.tiny-mokuhan.util.misc :as umizc]
            [org.panchromatic.tiny-mokuhan.util.regex :as uregex]
            [org.panchromatic.tiny-mokuhan.zip :as mzip]))

(def ^:private double-mustache
  {:open "{{"
   :close "}}"})

(defn- open-delim-matcher [delim template]
  (-> (uregex/quote delim)
      (str "([" "\\{" "\\&" "\\#" "\\/" "\\^" "\\>])?")
      re-pattern
      (re-matcher template)))

(defn- close-delim-matcher [delim template]
  (-> delim
      uregex/quote
      re-pattern
      (re-matcher template)))

(defn- update-delim-matchers
  ([matchers]
   (-> matchers
       (update :open-delim uregex/reset!)
       (update :close-delim uregex/reset!)))
  ([matchers template]
   (-> matchers
       (update :open-delim uregex/reset! template)
       (update :close-delim uregex/reset! template))))

(declare parse* parse-text parse-tag)

(defn- parse-text* [s]
  (let [m (re-matcher #"\r?\n" s)]
    (loop [s s
           result []]
      (let [m (uregex/reset! m s)]
        (if-let [[_ st ed] (uregex/re-find-pos m)]
          (recur (subs s ed)
                 (cond-> result
                   (not= 0 st) (conj (ast/text (subs s 0 st)))
                   true (conj (ast/newline))))
          (cond-> result
            (not= 0 (.length s)) (conj (ast/text (subs s 0 (.length s))))))))))

(defn- parse-text [template {{:keys [open-delim]} :matchers :as state}]
  (let [[_ od-st] (uregex/re-find-pos open-delim)
        [_ od-st2] (-> (uregex/reset! open-delim (subs template 1))
                       (uregex/re-find-pos))
        od-st2 (some-> od-st2 inc)
        end-pos (cond
                  (pos-int? od-st) od-st
                  (pos-int? od-st2) od-st2 ;; skip to next open delimiter
                  :else (.length template))
        items (parse-text* (subs template 0 end-pos))
        template' (subs template end-pos)]
    #(parse* template'
             (-> state
                 (update :location mzip/append-children items)
                 (update :matchers update-delim-matchers template')))))

(def ^:private key-pattern
  "[^\\s\\.]+")

(def ^:private variable-pattern
  (re-pattern (str "\\s*"
                   "(?<ks>" key-pattern "(?:\\." key-pattern ")*)"
                   "\\s*")))

(defn- try-parse-variable*
  [f template {{:keys [close-delim]} :matchers
               :keys [delimiters] :as state}]
  (let [[od od-st od-ed] (get-in state [:parse-tag :od])
        [cd cd-st cd-ed] (uregex/re-find-pos close-delim)]
    (when-let [[_ ks] (and (int? od-st) (int? cd-st)
                           (->> (subs template od-ed cd-st)
                                (re-matches variable-pattern)))]
      (let [item (f ks {:open od :close cd})
            template' (subs template cd-ed)]
        #(parse* template'
                 (-> (dissoc state :parse-tag)
                     (update :location zip/append-child item)
                     (update :matchers update-delim-matchers template')))))))

(def ^{:private true
       :arglists '([template {{:keys [close-delim]} :matchers
                              :keys [delimiters] :as state}])}
  try-parse-variable
  (partial try-parse-variable* ast/variable))

(def ^{:private true
       :arglists '([template {{:keys [close-delim]} :matchers
                              :keys [delimiters] :as state}])}
  try-parse-unescaped-variable
  (partial try-parse-variable* ast/unescaped-variable))

(defn- try-parse-triple-mustache-variable
  [template {:keys [delimiters] :as state}]
  (let [[od od-st od-ed] (get-in state [:parse-tag :od])
        [cd cd-st cd-ed] (-> (re-matcher #"\Q}}}\E" template)
                             uregex/re-find-pos)]
    (when-let [[_ ks] (and (int? od-st) (int? cd-st)
                           (= double-mustache delimiters)
                           (->> (subs template od-ed cd-st)
                                (re-matches variable-pattern)))]
      (let [item (ast/unescaped-variable ks {:open od :close cd})
            template' (subs template cd-ed)]
        #(parse* template'
                 (-> (dissoc state :parse-tag)
                     (update :location zip/append-child item)
                     (update :matchers update-delim-matchers template')))))))

(defn- parse-tag [template {{:keys [open close]} :delimiters
                            {:keys [open-delim close-delim]} :matchers
                            :keys [delimiters]
                            :as state}]
  (let [[[od sigil] od-st od-ed] (uregex/re-find-pos open-delim)
        state (assoc-in state [:parse-tag :od] [od od-st od-ed])]
    (or (case sigil
          "&" (try-parse-unescaped-variable template state)
          "{" (try-parse-triple-mustache-variable template state)
          nil (try-parse-variable template state))
        #(parse-text template
                     (-> (dissoc state :parse-tag)
                         (update :matchers update-delim-matchers))))))

(defn- parse* [template {{:keys [open-delim close-delim]} :matchers :as state}]
  (if (or (nil? template) (zero? (.length template)))
    (if (zero? (:nest-level state))
      (zip/root (:location state))
      (throw (ex-info "Invalid template" {})))

    (let [[_ od-st od-ed] (uregex/re-find-pos open-delim)
          state (update state :matchers update-delim-matchers)]
      (if (and (int? od-st) (zero? od-st))
        #(parse-tag template state)
        #(parse-text template state)))))

(def default-parser-options
  {:delimiters double-mustache})

(defn parse
  "Parse mustache template, then return AST.
  Some options are available:

  :delimiters - You can set the default delimiters.
                Default delimiters are {:open \"{{\" :close \"}}\"}
  "
  ([template]
   (parse template default-parser-options))

  ([template options]
   (let [{:keys [delimiters]
          :as options} (umizc/deep-merge default-parser-options options)
         state {:location (mzip/ast-zip (ast/mustache))
                :nest-level 0
                :default-delimiters delimiters
                :delimiters delimiters
                :matchers {:open-delim (-> (:open delimiters)
                                           (open-delim-matcher template))
                           :close-delim (-> (:close delimiters)
                                            (close-delim-matcher template))}}]
     (trampoline parse* template state))))
