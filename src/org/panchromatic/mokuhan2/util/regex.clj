(ns org.panchromatic.mokuhan2.util.regex
  (:refer-clojure :exclude [reset!]))

(defn reset!
  ([^java.util.regex.Matcher matcher]
   (.reset matcher))
  ([^java.util.regex.Matcher matcher ^String s]
   (.reset matcher s)))

(defn re-find-pos [^java.util.regex.Matcher matcher]
  (when-let [result (re-find matcher)]
    [result (.start matcher) (.end matcher)]))

(defn quote [s]
  (java.util.regex.Pattern/quote s))
