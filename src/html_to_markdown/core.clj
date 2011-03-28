(ns html-to-markdown.core
  (:use clojure.test))

(defn get-name [node] (name (:tag node)))
(defn get-attrs [node] (:attrs node)) 
(defn get-content [node] (:content node))

(defn html-to-markdown
  [html]
  (apply
   str
   (if (empty? html) ""
   (if (string? (first html))
     (concat (first html) (html-to-markdown (rest html)))
     (cond
      (= "p" (get-name (first html)))
      (concat "\n" (html-to-markdown (get-content (first html))) "\n"
	      (html-to-markdown (rest html)))
      (= "a" (get-name (first html)))
      (concat "[" (html-to-markdown (get-content (first html))) "]"
	      "(" (:href (get-attrs (first html))) ")"
	      (html-to-markdown (rest html)))
      (= "h1" (get-name (first html)))
      (concat "\n#" (html-to-markdown (get-content (first html))) "#\n"
	      (html-to-markdown (rest html)))
      (= "h2" (get-name (first html)))
      (concat "\n##" (html-to-markdown (get-content (first html))) "##\n"
	      (html-to-markdown (rest html)))
      (= "h3" (get-name (first html)))
      (concat "\n###" (html-to-markdown (get-content (first html))) "###\n"
	      (html-to-markdown (rest html)))
      (= "h4" (get-name (first html)))
      (concat "\n####" (html-to-markdown (get-content (first html))) "####\n"
	      (html-to-markdown (rest html)))
      (= "h5" (get-name (first html)))
      (concat "\n#####" (html-to-markdown (get-content (first html))) "#####\n"
	      (html-to-markdown (rest html)))
      (= "h6" (get-name (first html)))
      (concat "\n######" (html-to-markdown (get-content (first html))) "######\n"
	      (html-to-markdown (rest html)))
      (or (= "b" (get-name (first html)))
	  (= "strong" (get-name (first html))))
      (concat "**" (html-to-markdown (get-content (first html))) "**"
	      (html-to-markdown (rest html)))
      (or (= "i" (get-name (first html)))
	  (= "em" (get-name (first html))))
      (concat "*" (html-to-markdown (get-content (first html))) "*"
	      (html-to-markdown (rest html)))
      (= "blockquote" (get-name (first html)))
      (concat "\n\n> " (html-to-markdown (get-content (first html))) "\n"
	      (html-to-markdown (rest html)))
      (= "li" (get-name (first html)))
      (concat "\n- " (html-to-markdown (get-content (first html))) "\n"
	      (html-to-markdown (rest html)))
      (= "hr" (get-name (first html)))
      (concat "\n- - -\n" (html-to-markdown (rest html)))
      :else (concat
	     (html-to-markdown (get-content (first html)))
	     (html-to-markdown (rest html))))))))

(deftest p-test
  (is (= "\np test\n" (html-to-markdown [{:tag :p :content ["p test"]}]))))

(deftest b-test
  (is (= "**b test**" (html-to-markdown [{:tag :b :content ["b test"]}])))
  (is (= "**b test**" (html-to-markdown [{:tag :strong :content ["b test"]}]))))

(deftest i-test
  (is (= "_i test_") (html-to-markdown [{:tag :i :content ["i test"]}]))
  (is (= "_i test_") (html-to-markdown [{:tag :em :content ["i test"]}])))

(deftest a-test
  (is (= "[a](test)" (html-to-markdown [{:tag :a :attrs {:href "test"} :content ["a"]}])))
  (is (= "[as](test)" (html-to-markdown [{:tag :a :attrs {:href "test"} :content ["as"]}]))))

(deftest h-test
  (is (= "\n#Test#\n" (html-to-markdown [{:tag :h1 :content ["Test"]}])))
  (is (= "\n##Test##\n" (html-to-markdown [{:tag :h2 :content ["Test"]}])))
  (is (= "\n###Test###\n" (html-to-markdown [{:tag :h3 :content ["Test"]}])))
  (is (= "\n####Test####\n" (html-to-markdown [{:tag :h4 :content ["Test"]}])))
  (is (= "\n#####Test#####\n" (html-to-markdown [{:tag :h5 :content ["Test"]}]))) 
  (is (= "\n######Test######\n" (html-to-markdown [{:tag :h6 :content ["Test"]}]))))

(deftest hr-test
  (is (= "\n- - -\n" (html-to-markdown [{:tag :hr}]))))

(deftest bq-test
  (is (= "\n\n> Testing\n" (html-to-markdown [{:tag :blockquote :content ["Testing"]}]))))

(deftest l-test
  (is (= "\n- Test\n" (html-to-markdown [{:tag :li :content ["Test"]}]))))

(deftest markdown-test
  (a-test)
  (b-test)
  (bq-test)
  (h-test)
  (hr-test)
  (i-test)
  (l-test)
  (p-test))