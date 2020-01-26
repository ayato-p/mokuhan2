(ns org.panchromatic.mokuhan2.ast2-test
  (:require [org.panchromatic.mokuhan2.ast2 :as ast]
            [clojure.test :as t]))

(def tc
  {:delimiters {:open "{{" :close "}}"}
   :row        1
   :column     1
   :contexts   []})

(t/deftest variable-tag->mustache-str-test
  (t/is (= "{{foo}}"
           (-> (ast/variable-tag ["foo"] tc)
               (ast/variable-tag->mustache-str))))

  (t/is (= "{{foo.bar}}"
           (-> (ast/variable-tag ["foo" "bar"] tc)
               (ast/variable-tag->mustache-str)))))

(t/deftest unescaped-variable-tag->mustache-str-test
  (t/is (= "{{&foo}}"
           (-> (ast/unescaped-variable-tag ["foo"] tc)
               (ast/unescaped-variable-tag->mustache-str))))

  (t/is (= "{{&foo.bar}}"
           (-> (ast/unescaped-variable-tag ["foo" "bar"] tc)
               (ast/unescaped-variable-tag->mustache-str)))))

(t/deftest open-section-tag->mustache-str-test
  (t/is (= "{{#foo}}"
           (-> (ast/section-open-tag ["foo"] tc)
               (ast/section-open-tag->mustache-str))))

  (t/is (= "{{#foo.bar}}"
           (-> (ast/section-open-tag ["foo" "bar"] tc)
               (ast/section-open-tag->mustache-str)))))

(t/deftest close-section-tag->mustache-str-test
  (t/is (= "{{/foo}}"
           (-> (ast/section-close-tag ["foo"] tc)
               (ast/section-close-tag->mustache-str))))

  (t/is (= "{{/foo.bar}}"
           (-> (ast/section-close-tag ["foo" "bar"] tc)
               (ast/section-close-tag->mustache-str)))))

(t/deftest section->mustache-str-test
  (t/is (= "{{#foo}}{{bar}}{{/foo}}"
           (-> (ast/section (ast/section-open-tag ["foo"] tc)
                            (ast/section-close-tag ["foo"] tc)
                            [(ast/variable-tag ["bar"] tc)])
               (ast/section->mustache-str)))))

(t/deftest open-inverted-section-tag->mustache-str-test
  (t/is (= "{{^foo}}"
           (-> (ast/inverted-section-open-tag ["foo"] tc)
               (ast/inverted-section-open-tag->mustache-str))))

  (t/is (= "{{^foo.bar}}"
           (-> (ast/inverted-section-open-tag ["foo" "bar"] tc)
               (ast/inverted-section-open-tag->mustache-str)))))

(t/deftest inverted-section->mustache-str-test
  (t/is (= "{{^foo}}{{bar}}{{/foo}}"
           (-> (ast/inverted-section
                (ast/section-open-tag ["foo"] tc)
                (ast/section-close-tag ["foo"] tc)
                [(ast/variable-tag ["bar"] tc)])
               (ast/inverted-section->mustache-str)))))

(t/deftest partial->mustache-str-test
  (t/is (= "{{>foo}}"
           (-> (ast/partial "foo" tc)
               (ast/partial->mustache-str)))))

(t/deftest set-delimiter->mustache-str-test
  (t/is (= "{{=<< >>=}}"
           (-> (ast/set-delimiter {:open "<<" :close ">>"} tc)
               (ast/set-delimiter->mustache-str)))))

(t/deftest primitive->mustache-str-test
  (t/is (= "Hello"
           (-> (ast/text "Hello" tc)
               (ast/primitive->mustache-str))))

  (t/is (= "   "
           (-> (ast/whitespace "   " tc)
               (ast/primitive->mustache-str))))

  (t/is (= "\n"
           (-> (ast/newline "\n" tc)
               (ast/primitive->mustache-str)))))

(t/deftest complicated-pattern-test
  (let [ast (ast/syntax-tree
             [(ast/text "Hello," tc)
              (ast/whitespace " " tc)
              (ast/variable-tag ["name"] tc)
              (ast/text "." tc)
              (ast/newline "\r\n" tc)
              (ast/section
               (ast/section-open-tag ["flag"] tc)
               (ast/section-close-tag ["flag"] tc)
               [(ast/newline "\r\n" tc)
                (ast/text "Dear," tc)
                (ast/whitespace " " tc)
                (ast/variable-tag ["name"] tc)
                (ast/text "." tc)
                (ast/newline "\r\n" tc)])])]
    (t/is (= (str "Hello, {{name}}.\r\n"
                  "{{#flag}}\r\n"
                  "Dear, {{name}}.\r\n"
                  "{{/flag}}")
             (ast/syntax-tree->mustache-str ast)))))
