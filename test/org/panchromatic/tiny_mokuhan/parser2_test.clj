(ns org.panchromatic.tiny-mokuhan.parser2-test
  (:require [clojure.string :as str]
            [clojure.test :as t]
            [org.panchromatic.tiny-mokuhan.ast2 :as ast]
            [org.panchromatic.tiny-mokuhan.parser2 :as p]
            [org.panchromatic.tiny-mokuhan.reader :as reader]
            [org.panchromatic.tiny-mokuhan.zip2 :as mzip]
            [clojure.zip :as zip]))

(defmacro import-private-var
  ([qsym]
   `(if (qualified-symbol? ~qsym)
      (import-private-var (symbol (namespace ~qsym)) (symbol (name ~qsym)))
      (throw (IllegalArgumentException. "qsym should be qualified symbol"))))
  ([ns-sym name-sym]
   `(intern (ns-name *ns*)
            ~name-sym
            (ns-resolve ~ns-sym ~name-sym))))

(defmacro import-private-vars [qsyms]
  `(do
     ~@(for [qsym qsyms]
         `(import-private-var ~qsym))))

(import-private-vars
 [`p/lookahead-and-matched?
  `p/read-text
  `p/read-whitespace
  `p/read-newline])

(defn- test-reader
  ([s]
   (test-reader s 1))
  ([s n]
   (reader/pushback-reader (java.io.StringReader. s) n)))

(def ^:private default-delimiters
  {:open "{{" :close "}}"})

(def ^:private initial-state
  {:ast (mzip/ast-zip)
   :template-context {:delimiters default-delimiters
                      :row 1
                      :column 0
                      :standalone? true}})

(defn- read-rest [reader]
  (loop [sb (StringBuilder.)]
    (if-let [c (reader/read-char reader)]
      (recur (.append sb c))
      (.toString sb))))

(t/deftest lookahead-and-matched?-test
  (with-open [reader (test-reader "{{" 2)]
    (t/is (lookahead-and-matched? reader "{{"))
    (t/is (= (read-rest reader) "{{")))

  (with-open [reader (test-reader "{" 2)]
    (t/is (false? (lookahead-and-matched? reader "{{")))
    (t/is (= (read-rest reader) "{")))

  (with-open [reader (test-reader "<<<" 3)]
    (t/is (lookahead-and-matched? reader "<<<"))
    (t/is (= (read-rest reader) "<<<")))

  (with-open [reader (test-reader "<<||" 3)]
    (t/is (false? (lookahead-and-matched? reader "<<<")))
    (t/is (= (read-rest reader) "<<||"))))

(t/deftest read-text-test
  (t/are [s expected rest-str]
      (with-open [reader (test-reader s 2)]
        (and (= {:ast (ast/syntax-tree
                       [(ast/text expected (ast/template-context default-delimiters
                                                                 1
                                                                 0
                                                                 true))])
                 :template-context {:delimiters default-delimiters
                                    :row 1
                                    :column 5
                                    :standalone? false}}
                (-> (read-text reader initial-state)
                    (update :ast zip/root)))
             (= rest-str (read-rest reader))))
    "Hello" "Hello" ""
    "Hello " "Hello" " "
    "Hello\t" "Hello" "\t"
    "Hello\r\n" "Hello" "\r\n"
    "Hello\n" "Hello" "\n"
    "Hello{{" "Hello" "{{"
    "Hello{{name}}" "Hello" "{{name}}"
    "Hello world" "Hello" " world")

  (with-open [reader (test-reader "Hello{x{x" 2)]
    (t/is (= {:ast (ast/syntax-tree
                    [(ast/text "Hello{x{x" (ast/template-context default-delimiters
                                                                 1
                                                                 0
                                                                 true))])
              :template-context {:delimiters default-delimiters
                                 :row 1
                                 :column 9
                                 :standalone? false}}
             (-> (read-text reader initial-state)
                 (update :ast zip/root))))
    (t/is (= "" (read-rest reader)))))

(t/deftest read-whitespace-test
  (t/are [s expected rest-str]
      (with-open [reader (test-reader s 2)]
        (and (= {:ast (ast/syntax-tree
                       [(ast/whitespace " " (ast/template-context default-delimiters
                                                                  1
                                                                  0
                                                                  true))])
                 :template-context {:delimiters default-delimiters
                                    :row 1
                                    :column 1
                                    :standalone? true}}
                (-> (read-whitespace reader initial-state)
                    (update :ast zip/root)))
             (= rest-str (read-rest reader))))
    " " " " ""
    " x" " " "x"
    " \n" " " "\n"
    " \r\n" " " "\r\n"
    " {{foo}}" " " "{{foo}}")


  (with-open [reader (test-reader "  x" 2)]
    (t/is (= {:ast (ast/syntax-tree
                    [(ast/whitespace "  " (ast/template-context default-delimiters
                                                                1
                                                                0
                                                                true))])
              :template-context {:delimiters default-delimiters
                                 :row 1
                                 :column 2
                                 :standalone? true}}
             (-> (read-whitespace reader initial-state)
                 (update :ast zip/root))))
    (t/is (= "x" (read-rest reader)))))

(t/deftest read-newline-test
  (with-open [reader (test-reader "\r\nx" 2)]
    (t/is (= {:ast (ast/syntax-tree
                    [(ast/newline "\r\n" (ast/template-context default-delimiters
                                                               1
                                                               0
                                                               true))])
              :template-context {:delimiters default-delimiters
                                 :row 2
                                 :column 0
                                 :standalone? true}}
             (-> (read-newline reader initial-state)
                 (update :ast zip/root))))
    (t/is (= "x" (read-rest reader))))

  (with-open [reader (test-reader "\nx" 2)]
    (t/is (= {:ast (ast/syntax-tree
                    [(ast/newline "\n" (ast/template-context default-delimiters
                                                             1
                                                             0
                                                             true))])
              :template-context {:delimiters default-delimiters
                                 :row 2
                                 :column 0
                                 :standalone? true}}
             (-> (read-newline reader initial-state)
                 (update :ast zip/root))))
    (t/is (= "x" (read-rest reader)))))
