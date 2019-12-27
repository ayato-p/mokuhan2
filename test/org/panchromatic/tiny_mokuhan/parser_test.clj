(ns org.panchromatic.tiny-mokuhan.parser-test
  (:require [clojure.test :as t]
            [org.panchromatic.tiny-mokuhan.ast :as ast]
            [org.panchromatic.tiny-mokuhan.parser :as sut]))

(t/deftest text-parsing-test
  (t/testing "only text"
    (t/is (= (ast/mustache
              {:contents
               [(ast/text {:content "Hello"})]})
             (sut/parse "Hello")))

    (t/is (= (ast/mustache
              {:contents
               [(ast/text {:content "Hello"})
                (ast/newline)
                (ast/text {:content "World"})]})
             (sut/parse "Hello\nWorld")))

    (t/is (= (ast/mustache
              {:contents
               [(ast/text {:content "Hello "})
                (ast/newline)
                (ast/text {:content " World"})]})
             (sut/parse "Hello \n World")))))

(t/deftest variable-parsing-test
  (t/testing "balanced"
    (t/is (= (ast/mustache
              {:contents
               [(ast/variable {:keys "foo"
                               :open-delim "{{"
                               :close-delim "}}"})]})
             (sut/parse "{{foo}}")
             (sut/parse "{{ foo }}")
             (sut/parse "{{\tfoo\t}}")
             (sut/parse "{{\nfoo\n}}")))

    (t/is (= (ast/mustache
              {:contents
               [(ast/text {:content "Hello, "})
                (ast/variable {:keys "name"
                               :open-delim "{{"
                               :close-delim "}}"})]})
             (sut/parse "Hello, {{name}}"))))

  (t/testing "unbalanced"
    (t/is (= (ast/mustache
              {:contents
               [(ast/text {:content "{"})
                (ast/variable {:keys "foo"
                               :open-delim "{{"
                               :close-delim "}}"})]})
             (sut/parse "{{{foo}}")))

    (t/is (= (ast/mustache
              {:contents
               [(ast/text {:content "{"})
                (ast/text {:content "{"})
                (ast/variable {:keys "foo"
                               :open-delim "{{"
                               :close-delim "}}"})]})
             (sut/parse "{{{{foo}}")))

    (t/is (= (ast/mustache
              {:contents
               [(ast/text {:content "{{ "})
                (ast/variable {:keys "foo"
                               :open-delim "{{"
                               :close-delim "}}"})]})
             (sut/parse "{{ {{foo}}")))))

(t/deftest default-delimiters-test
  (t/is (= (ast/mustache
            {:contents [(ast/variable {:keys "foo"
                                       :open-delim "{{"
                                       :close-delim "}}"})]})
           (sut/parse "{{foo}}")
           (sut/parse "{{foo}}" {:delimiters {:open "{{" :close "}}"}})))

  (let [options {:delimiters {:open "<<" :close ">>"}}]
    (t/is (= (ast/mustache
              {:contents [(ast/variable {:keys "foo"
                                         :open-delim "<<"
                                         :close-delim ">>"})]})
             (sut/parse "<<foo>>" options)
             (sut/parse "<< foo >>" options)))

    (t/is (= (ast/mustache
              {:contents [(ast/text {:content "{{foo}}"})]})
             (sut/parse "{{foo}}" options)))

    (t/is (= (ast/mustache
              {:contents [(ast/text {:content "<< "})
                          (ast/variable {:keys "foo"
                                         :open-delim "<<"
                                         :close-delim ">>"})]})
             (sut/parse "<< << foo >>" options)))))
