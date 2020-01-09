(ns org.panchromatic.tiny-mokuhan.parser2
  (:require [org.panchromatic.tiny-mokuhan.ast2 :as ast]
            [org.panchromatic.tiny-mokuhan.reader :as reader]
            [org.panchromatic.tiny-mokuhan.util.misc :as umisc]
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

(defn- parse-text [reader state]
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

(defn- parse-whitespace [reader state]
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

(defn- parse-newline [reader state]
  (let [newln (read-newline* reader)
        newln-node (ast/newline newln (state->template-context state))]
    (-> state
        (update-in [:ast] mzip/append-primitive newln-node)
        (assoc-in [:template-context :column] 1)
        (update-in [:template-context :row] inc)
        (assoc-in [:template-context :standalone?] true))))

(let [state {:begin {:read-ws :before-read-keys
                     :read-char :read-keys}
             :before-read-keys {:read-ws :before-read-keys
                                :read-char :read-keys}
             :read-keys {:read-char :read-keys
                         :read-ws :after-read-keys
                         :read-delim :end}
             :after-read-keys {:read-ws :after-read-keys
                               :read-delim :end}
             :end {}}]
  (defn- next-read-keys-state [current action]
    (get-in state [current action])))

(def ^:private valid-read-keys-state
  #{:begin
    :before-read-keys
    :read-keys
    :after-read-keys
    :end})

(defn- read-keys [reader close-delim]
  (loop [^StringBuilder sb (StringBuilder.)
         state :begin
         result {:ks [] :read-cnt 0}]
    (let [c (reader/read-char reader)]
      (cond
        (= :end state)
        (do
          (reader/unread-char reader c)
          result)

        (nil? state)
        {:err "Invalid tag name"}

        (nil? c)
        {:err "Unclosed tag"}

        (contains? #{\space \tab \　} c)
        (recur sb
               (next-read-keys-state state :read-ws)
               (update result :read-cnt inc))

        (= (.charAt close-delim 0) c)
        (let [red (reader/read-chars reader (.length (subs close-delim 1)))
              close-delim' (apply str c red)]
          (if (= close-delim' close-delim)
            (do
              (reader/unread-chars reader close-delim')
              (recur nil
                     (next-read-keys-state state :read-delim)
                     (update result :ks conj (.toString sb))))
            (do
              (reader/unread-chars reader red)
              (recur (.append sb c)
                     (next-read-keys-state state :read-char)
                     (update result :read-cnt inc)))))

        (= \. c)
        (recur (StringBuilder.)
               (next-read-keys-state state :read-char)
               (-> result
                   (update :ks conj (.toString sb))
                   (update :read-cnt inc)))

        :else
        (recur (.append sb c)
               (next-read-keys-state state :read-char)
               (update result :read-cnt inc))))))

(defn- parse-variable-tag [reader state]
  (let [{open-delim :open close-delim :close} (get-in state [:template-context :delimiters])
        _ (reader/skip reader (.length open-delim))
        {:keys [ks read-cnt err]} (read-keys reader close-delim)
        _ (reader/skip reader (.length close-delim))]
    (if-not err
      (let [variable-tag-node (ast/variable-tag ks (state->template-context state))]
        (-> state
            (update-in [:ast] mzip/append-primitive variable-tag-node)
            (update-in [:template-context :column] + (.length open-delim) read-cnt (.length close-delim))
            (assoc-in [:template-context :standalone?] false)))

      {:error {:type :org.panchromatic.tiny-mokuhan/parse-variable-tag-error
               :message err
               :occurred (-> (get-in state [:template-context])
                             (select-keys [:row :column]))}})))

(defn parse-unescaped-variable-tag [reader state]
  (let [{open-delim :open close-delim :close} (get-in state [:template-context :delimiters])
        _ (reader/skip reader (.length open-delim))
        ensure-unescaped-variable? (= \& (reader/read-char reader))
        {:keys [ks read-cnt err]} (read-keys reader close-delim)
        _ (reader/skip reader (.length close-delim))]
    (if (and ensure-unescaped-variable? (nil? err))
      (let [unescaped-variable-tag-node (ast/unescaped-variable-tag ks (state->template-context state))]
        (-> state
            (update-in [:ast] mzip/append-primitive unescaped-variable-tag-node)
            (update-in [:template-context :column] + (.length open-delim) 1 read-cnt (.length close-delim))
            (assoc-in [:template-context :standalone?] false)))

      {:error {:type :org.panchromatic.tiny-mokuhan/parse-unescaped-variable-tag-error
               :message err
               :occurred (-> (get-in state [:template-context])
                             (select-keys [:row :column]))}})))

(def default-parse-options
  {:delimiters {:open "{{" :close "}}"}})

(defn parse* [^java.io.Reader reader options]
  (let [{:keys [delimiters] :as optins} (umisc/deep-merge default-parse-options options)
        state {:ast (mzip/ast-zip)
               :template-context {:delimiters delimiters :row 1 :column 1 :standalone? true}}]
    (with-open [reader (reader/pushback-reader reader (.length (:open delimiters)))]
      (loop [reader reader
             {:keys [error] :as state} state]
        (if error
          state
          (let [open-delim (get-in state [:template-context :delimiters :open])]
            (if-let [c (reader/read-char reader)]
              (do
                (reader/unread-char reader c)
                (if (and (= (.charAt open-delim 0) c)
                         (lookahead-and-matched? reader (subs open-delim 1)))
                  (recur reader (parse-variable-tag reader state))
                  (case c
                    (\return \newline)
                    (recur reader (parse-newline reader state))
                    (\space \tab)
                    (recur reader (parse-whitespace reader state))
                    (recur reader (parse-text reader state)))))
              (update state :ast mzip/complete))))))))

(defprotocol Parsable
  (parse [this] [this options]))

(extend-protocol Parsable
  java.lang.String
  (parse
    ([s]
     (parse s default-parse-options))
    ([s options]
     (with-open [reader (java.io.StringReader. s)]
       (parse* reader options)))))
