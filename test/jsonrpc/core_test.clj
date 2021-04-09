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
  [_ a b]
  (+ a b))


(defn user-create
  [_ {:keys [name age email]}]
  {:id 100
   :name name
   :age age
   :email email})


(s/def :user/create.in
  (s/keys :req-un [:user/name
                   :user/age
                   :user/email]))

(s/def :user/create.out
  (s/keys :req-un [:user/id
                   :user/name
                   :user/age
                   :user/email]))


(def config
  {:methods
   {:math/sum
    {:handler #'rpc-sum
     :spec-in :math/sum.in
     :spec-out :math/sum.out}

    :user/create
    {:hander user-create
     :spec-in :user/create.in
     :spec-out :user/create.out}}})


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


(deftest test-handler-map-params

  (let [rpc {:id 1
             :method "user/create"
             :params {:name "Ivan"
                      :age 35
                      :email "test@testc.com"}
             :version "2.0"}
        request {:params rpc}
        handler (server/make-handler config)

        response (handler request)]

    (is (= {:status 200
            :body
            {:id 1
             :jsonrpc "2.0"
             :result {:id 100
                      :name "Ivan"
                      :age 35
                      :email "test@testc.com"}}}
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
