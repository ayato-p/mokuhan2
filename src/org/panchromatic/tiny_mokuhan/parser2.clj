(ns org.panchromatic.tiny-mokuhan.parser2
  (:require [org.panchromatic.tiny-mokuhan.ast2 :as ast]
            [org.panchromatic.tiny-mokuhan.reader :as reader]
            [org.panchromatic.tiny-mokuhan.zip2 :as mzip]))

(defn- lookahead-and-matched? [reader s]
  (let [read (repeatedly (.length s) #(reader/read-char reader))
        matched (= s (apply str read))]
    (doseq [c (filter some? (reverse read))]
      (reader/unread-char reader c))
    matched))

(let [extractor (juxt :delimiters :row :column :standalone?)]
  (defn- state->template-context [{:keys [template-context] :as state}]
    (->> (extractor template-context)
         (apply ast/template-context))))

(defn- read-text* [reader open-delim]
  (loop [^StringBuilder sb (StringBuilder.)
         text? true]
    (if-let [c (and text? (reader/read-char reader))]
      (cond
        (contains? #{\space \tab \return \newline} c)
        #_> (do (reader/unread-char reader c)
                (recur sb false))
        (= (.charAt open-delim 0) c)
        #_> (if (lookahead-and-matched? reader (subs open-delim 1))
              (do (reader/unread-char reader c)
                  (recur sb false))
              (recur (.append sb c) text?))
        :else
        #_> (recur (.append sb c) text?))
      (.toString sb))))

(defn- read-text [reader state]
  (let [text (read-text* reader (get-in state [:template-context :delimiters :open]))
        text-node (ast/text text (state->template-context state))]
    (-> state
        (update-in [:ast] mzip/append-primitive text-node)
        (update-in [:template-context :column] + (.length text))
        (assoc-in [:template-context :standalone?] false))))

(defn- read-whitespace* [reader]
  (loop [^StringBuilder sb (StringBuilder.)
         whitespace? true]
    (let [c (and whitespace? (reader/read-char reader))]
      (case c
        (\space \tab)
        (recur (.append sb c) whitespace?)
        (do (when c (reader/unread-char reader c))
            (.toString sb))))))

(defn- read-whitespace [reader state]
  (let [whitespaces (read-whitespace* reader)
        ws-node (ast/whitespace whitespaces (state->template-context state))]
    (-> state
        (update-in [:ast] mzip/append-primitive ws-node)
        (update-in [:template-context :column] + (.length whitespaces)))))

(defn- read-newline* [reader]
  (loop [^StringBuilder sb (StringBuilder.)
         saw-cr false]
    (let [c (reader/read-char reader)]
      (case c
        \return
        (recur sb true)
        \newline
        (-> (cond-> sb saw-cr (.append \return))
            (.append c)
            (.toString))))))

(defn- read-newline [reader state]
  (let [newln (read-newline* reader)
        newln-node (ast/newline newln (state->template-context state))]
    (-> state
        (update-in [:ast] mzip/append-primitive newln-node)
        (assoc-in [:template-context :column] 0)
        (update-in [:template-context :row] inc)
        (assoc-in [:template-context :standalone?] true))))
