(ns org.panchromatic.tiny-mokuhan.util.regex)

(defn reset!
  ([^java.util.regex.Matcher matcher]
   (.reset matcher))
  ([^java.util.regex.Matcher matcher ^String s]
   (.reset matcher s)))

(defn re-find-pos [^java.util.regex.Matcher matcher]
  (when-let [result (re-find matcher)]
    [result (.start matcher) (.end matcher)]))
