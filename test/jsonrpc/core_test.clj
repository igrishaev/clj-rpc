(ns jsonrpc.core-test
  (:require
   [jsonrpc.spec :as spec]
   [jsonrpc.server :as server]

   [clojure.spec.alpha :as s]
   [clojure.test :refer [deftest is]]))


(s/def :math/sum.in
  (s/tuple number? number?))

(s/def :math/sum.out
  number?)


(defn rpc-sum
  [[a b] _]
  (+ a b))


(def config
  {:methods
   {:math/sum
    {:title "Sum numbers"
     :description "Some long description of the method"
     :handler #'rpc-sum
     :spec-in :math/sum.in
     :spec-out :math/sum.out}}})


(deftest test-handler-ok

  (let [rpc {:id 1
             :method "math/sum"
             :params [1 2]
             :version "2.0"}
        request {:params rpc}
        handler (server/make-handler config)

        response (handler request)]

    (is (= {:status 200
            :body {:id 1 :version "2.0" :result 3}}
           response))))


(deftest test-handler-not-found

  (let [rpc {:id 1
             :method "math/dunno"
             :params [1 2]
             :version "2.0"}
        request {:params rpc}
        handler (server/make-handler config)

        response (handler request)]

    (is (= {:status 404
            :body
            {:id 1
             :jsonrpc "2.0"
             :error {:code -32601
                     :message "Method not found"
                     :data {:method :math/dunno}}}}

           response))))


(deftest test-handler-notify-ok

  (let [rpc {:method "math/sum"
             :params [1 2]
             :version "2.0"}
        request {:params rpc}
        handler (server/make-handler config)

        response (handler request)]

    (is (= {:status 200 :body nil}
           response))))


(deftest test-handler-wrong-params

  (let [rpc {:id 1
             :method "math/sum"
             :params [1 nil]
             :version "2.0"}
        request {:params rpc}
        handler (server/make-handler config)

        response (handler request)]

    (is (= {:status 400
            :body
            {:id 1
             :jsonrpc "2.0"
             :error {:code -32602
                     :message "Invalid params"
                     :data
                     {:method :math/sum
                      :explain
                      "nil - failed: number? in: [1] at: [1] spec: :math/sum.in\n"}}}}

           response))))
