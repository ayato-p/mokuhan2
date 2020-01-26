(ns org.panchromatic.mokuhan2.parser.internal
  (:require [org.panchromatic.mokuhan2.ast2 :as ast]
            [org.panchromatic.mokuhan2.reader :as reader]
            [org.panchromatic.mokuhan2.util.string :as ustr]
            [org.panchromatic.mokuhan2.zip2 :as mzip]))

(defn make-initial-state
  [{:keys [delimiters] :as options}]
  {:ast (mzip/ast-zip)
   :template-context {:delimiters {}
                      :row 1
                      :column 1
                      :contexts []
                      :line-nodes []}})

(defn- tag-contains? [node-types]
  (reduce (fn [_ t]
            (if (contains? ast/tags t)
              (reduced true)
              false))
          false
          node-types))

(defn- standalone? [node-types]
  (reduce (fn [_ t]
            (if (= ::ast/whitespace t)
              true
              (reduced false)))
          true
          node-types))

(defn lookahead-and-matched? [reader s]
  (let [read (reader/read-chars reader (ustr/length s))
        _ (reader/unread-chars reader read)]
    (= s (apply str read))))

(defn- ->min-tc [template-context]
  (select-keys template-context [:delimiters :row :column :contexts]))

(defn- read-text [reader open-delim]
  (loop [sb (ustr/string-builder)
         read-cnt 0
         text? true]
    (if-let [c (and text? (reader/read-char reader))]
      (cond
        (contains? #{\space \tab \return \newline} c)
        (do
          (reader/unread-char reader c)
          (recur sb read-cnt false))

        (= (ustr/char-at open-delim 0) c)
        (if (lookahead-and-matched? reader (subs open-delim 1))
          (do
            (reader/unread-char reader c)
            (recur sb read-cnt false))
          (recur (ustr/append sb c) (inc read-cnt) text?))

        :else
        (recur (ustr/append sb c) (inc read-cnt) text?))
      {:text (str sb) :read-cnt read-cnt})))

(defn parse-text [reader {:keys [template-context] :as state}]
  (let [{:keys [text read-cnt]} (read-text reader (get-in template-context [:delimiters :open]))
        line-nodes (:line-nodes template-context)
        text-node (ast/text text (->min-tc template-context))]
    (-> state
        (cond-> (and (not (standalone? line-nodes)) (tag-contains? line-nodes))
          (update-in [:ast] mzip/look-behind))
        (update-in [:ast] mzip/append-node text-node)
        (update-in [:template-context :column] + read-cnt)
        (update-in [:template-context :line-nodes] conj (:type text-node)))))

(defn- read-whitespace [reader]
  (loop [sb (ustr/string-builder)
         read-cnt 0]
    (let [c (reader/read-char reader)]
      (case c
        (\space \tab \　)
        (recur (.append sb c) (inc read-cnt))
        (do
          (when c (reader/unread-char reader c))
          {:whitespaces (str sb) :read-cnt read-cnt})))))

(defn parse-whitespace [reader {:keys [template-context] :as state}]
  (let [{:keys [whitespaces read-cnt]} (read-whitespace reader)
        ws-node (ast/whitespace whitespaces (->min-tc template-context))]
    (-> state
        (update-in [:ast] mzip/append-node ws-node)
        (update-in [:template-context :column] + read-cnt)
        (update-in [:template-context :line-nodes] conj (:type ws-node)))))

(defn- read-newline [reader]
  (loop [^StringBuilder sb (ustr/string-builder)
         saw-cr false]
    (let [c (reader/read-char reader)]
      (case c
        \return
        (recur sb true)
        \newline
        (-> (cond-> sb saw-cr (ustr/append \return))
            (ustr/append c)
            str)))))

(defn parse-newline [reader {:keys [template-context] :as state}]
  (let [newln (read-newline reader)
        newln-node (ast/newline newln (->min-tc template-context))]
    (-> state
        (update-in [:ast] mzip/append-node newln-node)
        (assoc-in [:template-context :column] 1)
        (update-in [:template-context :row] inc)
        (assoc-in [:template-context :line-nodes] []))))

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
  (loop [sb (ustr/string-builder)
         state :begin
         result {:ks [] :read-cnt 0}]
    (let [c (reader/read-char reader)]
      (cond
        (= :end state)
        (do
          (reader/unread-char reader c)
          result)

        (nil? state)
        {:err :invalid-tag-name}

        (nil? c)
        {:err :unclosed-tag}

        (contains? #{\space \tab} c)
        (recur sb
               (next-read-keys-state state :read-ws)
               (update result :read-cnt inc))

        (= (ustr/char-at close-delim 0) c)
        (let [red (reader/read-chars reader (ustr/length (subs close-delim 1)))
              close-delim' (apply str c red)]
          (if (= close-delim' close-delim)
            (do
              (reader/unread-chars reader close-delim')
              (recur nil
                     (next-read-keys-state state :read-delim)
                     (update result :ks conj (str sb))))
            (do
              (reader/unread-chars reader red)
              (recur (ustr/append sb c)
                     (next-read-keys-state state :read-char)
                     (update result :read-cnt inc)))))

        (= \. c)
        (recur (ustr/string-builder)
               (next-read-keys-state state :read-char)
               (-> result
                   (update :ks conj (str sb))
                   (update :read-cnt inc)))

        :else
        (recur (ustr/append sb c)
               (next-read-keys-state state :read-char)
               (update result :read-cnt inc))))))

(defn- read-delimiter [reader delim]
  (let [delim (->> (ustr/length delim)
                   (reader/read-chars reader)
                   (keep identity)
                   (apply str))]
    {:read-cnt (.length delim)
     :delim delim}))

(defn- parse-error [cause template-context]
  {:error {:type :org.panchromatic.mokuhan2/parse-error
           :cause cause
           :occurred (select-keys template-context [:row :column :contexts])}})

(defn- tag-contains? [node-types]
  (reduce (fn [_ t]
            (if (contains? ast/tags t)
              (reduced true)
              false))
          false
          node-types))

(defn- standalone? [node-types]
  (reduce (fn [_ t]
            (if (= ::ast/whitespace t)
              true
              (reduced false)))
          true
          node-types))

(defn parse-variable-tag [reader {:keys [template-context] :as state}]
  (let [{{open-delim :open close-delim :close} :delimiters
         :keys [line-nodes]} template-context
        o (read-delimiter reader open-delim)
        {:as k :keys [ks read-cnt err]} (read-keys reader close-delim)
        c (read-delimiter reader close-delim)]
    (if-not err
      (let [standalone (standalone? line-nodes)
            tc (-> (->min-tc template-context)
                   (assoc :standalone? standalone))
            variable-tag-node (ast/variable-tag ks tc)]
        (-> state
            (cond-> (and (not standalone) (tag-contains? line-nodes))
              (update-in [:ast] mzip/look-behind))
            (update-in [:ast] mzip/append-node variable-tag-node)
            (update-in [:template-context :column] + (apply + (map :read-cnt [o k c])))
            (update-in [:template-context :line-nodes] conj (:type variable-tag-node))))

      (parse-error err template-context))))

(defn parse-unescaped-variable-tag [reader {:keys [template-context] :as state}]
  (let [{{open-delim :open close-delim :close} :delimiters
         :keys [line-nodes]} template-context
        o (read-delimiter reader open-delim)
        ensure-unescaped-variable? (= \& (reader/read-char reader))
        {:as k :keys [ks read-cnt err]} (read-keys reader close-delim)
        c (read-delimiter reader close-delim)]
    (if (and ensure-unescaped-variable? (nil? err))
      (let [standalone (standalone? line-nodes)
            tc (-> (->min-tc template-context)
                   (assoc :standalone? standalone))
            unescaped-variable-tag-node (ast/unescaped-variable-tag ks tc)]
        (-> state
            (cond-> (and (not standalone) (tag-contains? line-nodes))
              (update-in [:ast] mzip/look-behind))
            (update-in [:ast] mzip/append-node unescaped-variable-tag-node)
            (update-in [:template-context :column] + (apply + 1 (map :read-cnt [o k c])))
            (update-in [:template-context :line-nodes] conj (:type unescaped-variable-tag-node))))

      (parse-error err template-context))))

(defn parse-section-open-tag [reader {:keys [template-context] :as state}]
  (let [{{open-delim :open close-delim :close} :delimiters
         :keys [line-nodes]} template-context
        o (read-delimiter reader open-delim)
        ensure-open-section? (= \# (reader/read-char reader))
        {:as k :keys [ks read-cnt err]} (read-keys reader close-delim)
        c (read-delimiter reader close-delim)]
    (if (and ensure-open-section? (nil? err))
      (let [standalone (standalone? line-nodes)
            tc (-> (->min-tc template-context)
                   (assoc :standalone? standalone))
            section-open-tag-node (ast/section-open-tag ks tc)]
        (-> state
            (cond-> (and (not standalone) (tag-contains? line-nodes))
              (update-in [:ast] mzip/look-behind))
            (update-in [:ast] mzip/open-section section-open-tag-node)
            (update-in [:template-context :contexts] conj ks)
            (update-in [:template-context :column] + (apply + 1 (map :read-cnt [o k c])))
            (update-in [:template-context :line-nodes] conj (:type section-open-tag-node))))

      (parse-error err template-context))))

(defn parse-section-close-tag [reader {:keys [template-context] :as state}]
  (let [{{open-delim :open close-delim :close} :delimiters
         :keys [line-nodes contexts]} template-context
        current-context (peek contexts)
        o (read-delimiter reader open-delim)
        ensure-close-section? (= \/ (reader/read-char reader))
        {:as k :keys [ks read-cnt err]} (read-keys reader close-delim)
        c (read-delimiter reader close-delim)]
    (cond
      (and ensure-close-section? (= current-context ks) (nil? err))
      (let [standalone (standalone? line-nodes)
            tc (-> (->min-tc template-context)
                   (update :contexts pop)
                   (assoc :standalone? standalone))
            section-close-tag-node (ast/section-close-tag ks tc)]
        (-> state
            (cond-> (and (not standalone) (tag-contains? line-nodes))
              (update-in [:ast] mzip/look-behind))
            (update-in [:ast] mzip/close-section section-close-tag-node)
            (update-in [:template-context :contexts] pop)
            (update-in [:template-context :column] + (apply + 1 (map :read-cnt [o k c])))
            (update-in [:template-context :line-nodes] conj (:type section-close-tag-node))))

      (and (nil? err) (not= current-context ks))
      (parse-error :unclosed-section template-context)

      :else
      (parse-error err template-context))))

(let [ws #{\space \tab \　}]
  (defn- whitespace? [c]
    (contains? ws c)))

(let [nl #{\return \newline}]
  (defn- newline? [c]
    (contains? nl c)))

(defn parse [^java.io.PushbackReader reader state]
  (loop [reader reader
         {:keys [error] :as state} state]
    (if error
      state
      (let [open-delim (get-in state [:template-context :delimiters :open])
            ;; for choosing a parser
            c (reader/read-char reader)
            _ (reader/unread-char reader c)]
        (cond
          (whitespace? c)
          (recur reader (parse-whitespace reader state))

          (newline? c)
          (recur reader (parse-newline reader state))

          (nil? c)
          (update state :ast mzip/complete)

          (= (ustr/char-at open-delim 0) c)
          (let [open-delim' (:delim (read-delimiter reader open-delim))
                sigil (reader/read-char reader)
                _ (reader/unread-chars reader (cond-> open-delim' sigil (str sigil)))]
            (if (= open-delim open-delim')
              (case sigil
                \&
                (recur reader (parse-unescaped-variable-tag reader state))
                \#
                (recur reader (parse-section-open-tag reader state))
                \/
                (recur reader (parse-section-close-tag reader state))
                (recur reader (parse-variable-tag reader state)))

              (recur reader (parse-text reader state))))

          :else
          (recur reader (parse-text reader state)))))))
