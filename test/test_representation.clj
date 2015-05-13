(ns test-representation
  (:require [midje.sweet :refer :all]
            [liberator.representation :refer :all]))

;; test for issue #19
;; https://github.com/clojure-liberator/liberator/pull/19

(defn- pr-str-dup [x]
  (binding [*print-dup* true]
    (pr-str x)))

(facts "Can produce representations from map"
       (let [entity (sorted-map :foo "bar" :baz "qux")]
         (tabular "Various media types are supported"
                  (as-response entity {:representation {:media-type ?media-type :charset "UTF-8"}})
                  => {:body ?body :headers { "Content-Type" (str ?media-type ";charset=UTF-8")}}
                  ?media-type   ?body
                  "text/csv"    "name,value\r\n:baz,qux\r\n:foo,bar\r\n"
                  "text/tab-separated-values" "name\tvalue\r\n:baz\tqux\r\n:foo\tbar\r\n"
                  "text/plain"  "baz=qux\r\nfoo=bar"
                  "text/html"   "{:baz \"qux\", :foo \"bar\"}"
                  "application/json" (clojure.data.json/write-str entity)
                  "application/clojure" (pr-str-dup entity)
                  "application/edn" (pr-str entity))))

(facts "Can produce representations from a seq of maps"
       (let [entity [(sorted-map :foo 1 :bar 2) (sorted-map :foo 2 :bar 3)]]
         (tabular "Various media types are supported"
                  (as-response entity {:representation {:media-type ?media-type :charset "UTF-8"}})
                  => {:body ?body :headers { "Content-Type" (str ?media-type ";charset=UTF-8")}}
                  ?media-type   ?body
                  "text/csv"    "bar,foo\r\n2,1\r\n3,2\r\n"
                  "text/tab-separated-values" "bar\tfoo\r\n2\t1\r\n3\t2\r\n"
                  "text/plain"  "bar=2\r\nfoo=1\r\n\r\nbar=3\r\nfoo=2"
                  "text/html"   "[{:bar 2, :foo 1} {:bar 3, :foo 2}]"
                  "application/json" (clojure.data.json/write-str entity)
                  "application/clojure" (pr-str-dup entity)
                  "application/edn" (pr-str entity))))


(facts "Can give ring response map to override response values"
   (facts "returns single ring response unchanged"
     (let [response {:status 123 
                     :headers {"Content-Type" "application/json;charset=UTF-8"
                               "X-Foo" "Bar"}
                     :body "123" }]
       (as-response (ring-response response) {}) => response))
   (facts "delegates to default response generation when value is given"
         (fact "for strings"
           (as-response (ring-response "foo" {}) {}) => (as-response "foo" {}))
         (fact "for maps"
           (let [ctx {:representation {:media-type "application/json"}}]
             (as-response (ring-response {:a 1} {}) ctx)
             => (as-response {:a 1} ctx))))
       (facts "lets override response attributes"
         (fact "all attributes"
           (let [overidden {:body "body"
                           :headers ["Content-Type" "application/foo"]
                           :status 999}]
           (as-response (ring-response "foo" overidden)
                        {:status 200}) => overidden))
         (facts "some attributes"
           (facts "status"
             (as-response (ring-response "foo" {:status 999}) {:status 200})
             => (contains {:status 999}))
           (facts "header merged"
             (as-response (ring-response "foo" {:headers {"X-Foo" "bar"}})
               {:status 200})
             => (contains {:headers {"X-Foo" "bar"
                                     "Content-Type" "text/plain;charset=UTF-8"}})))))
