(ns org.panchromatic.tiny-mokuhan.parser
  (:refer-clojure :exclude [read-line])
  (:require [clojure.zip :as zip]
            [org.panchromatic.tiny-mokuhan.ast :as ast]
            [org.panchromatic.tiny-mokuhan.util.regex :as uregex]
            [org.panchromatic.tiny-mokuhan.zip :as mzip]))

(def ^:private sigils
  ["\\&" "\\#" "\\/" "\\^" "\\>"])

;;; {{name}}   -> variable
;;; {{{name}}} -> unescaped variable / when only delimiters are defaults
;;; {{&name}} -> unescaped variable
;;; {{#persone}} <-> {{/person}} -> section
;;;   false or empty list -> delete
;;;   non empty list -> repeat
;;;   lambda -> call function
;;;   non-false -> context
;;; {{^name}} <-> {{/name}} -> inverted variable
;;; {{! blah }} -> comment
;;; {{> box}} -> partial
;;; {{=<% %>=}} -> set delimiter

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
                   (not= 0 st) (conj (ast/->Text (subs s 0 st)))
                   true (conj (ast/->NewLine))))
          (cond-> result
            (not= 0 (.length s)) (conj (ast/->Text (subs s 0 (.length s))))))))))

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

(def ^:private id-pattern
  "[a-zA-Z][a-zA-Z0-9]+")

(def ^:private variable-pattern
  (re-pattern (str "\\s*"
                   "(?<id>" id-pattern "(?:\\." id-pattern ")*)"
                   "\\s*")))

(defn- parse-tag [template {{:keys [open close]} :delimiters
                            {:keys [open-delim close-delim]} :matchers
                            :as state}]
  (let [[_ od-st od-ed] (uregex/re-find-pos open-delim)
        [_ cd-st cd-ed] (uregex/re-find-pos close-delim)]

    (if-let [[_ id] (->> (subs template od-ed cd-st)
                         (re-matches variable-pattern))]
      (let [item (ast/->Variable id open close)
            template' (subs template cd-ed)]
        #(parse* template'
                 (-> state
                     (update :location zip/append-child item)
                     (update :matchers update-delim-matchers template'))))
      #(parse-text template
                   (update state :matchers update-delim-matchers)))))

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

(defn parse [template]
  (let [state {:location (mzip/ast-zip (ast/->Mustache []))
               :nest-level 0
               :delimiters {:open "{{"
                            :close "}}"}
               :matchers {:open-delim (re-matcher #"\Q{{\E" template)
                          :close-delim (re-matcher #"\Q}}\E" template)}}]
    (trampoline parse* template state)))
