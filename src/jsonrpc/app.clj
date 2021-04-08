(ns jsonrpc.app
  (:require

   [clojure.spec.alpha :as s]

   [jsonrpc.server :as server]

   [ring.adapter.jetty :refer [run-jetty]]

   [ring.middleware.params :refer [wrap-params]]
   [ring.middleware.keyword-params :refer [wrap-keyword-params]]
   [ring.middleware.json :refer [wrap-json-params wrap-json-response]]))



(defn add [[a b] _]
  (+ a b))


(s/def ::add.in (s/tuple int? int?))

(s/def ::add.out int?)



(def rpc-methods
  {:math/add
   {:title "Add"
    :description "Sums to numbers"
    :handler add
    :spec-in ::add.in
    :spec-out ::add.out}})


(def rpc-handler
  (server/make-handler {} rpc-methods))


(def app
  (-> rpc-handler
      wrap-keyword-params
      wrap-json-params
      wrap-params
      wrap-json-response))


(defn start-app []
  (run-jetty app {:port 8080 :join? false}))
