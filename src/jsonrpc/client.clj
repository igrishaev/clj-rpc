(ns jsonrpc.client
  (:require
   [clj-http.client :as client]

   ;; [clojure.string :as str]
   ;; [clojure.spec.alpha :as s]
   ))


(defn get-ns [kw]
  (-> kw namespace keyword))


(def config-defaults
  {:rpc/fn-before-send identity
   :rpc/fn-id :int
   :rpc/multi? false
   :rpc/fn-dispatch-service get-ns
   :rpc/notify? false
   :method :post
   :headers {:user-agent "jsonrpc.client"}
   :socket-timeout 5000
   :connection-timeout 5000
   :throw-exceptions? true
   :as :json
   :content-type :json})


(def config
  {:rpc/method-options {:users/delete {:rpc/notify? true}}

   :url "http://test.com/test"
   :auth ["user" "pass"]})


(def config-multi
  {:rpc/multi? true
   :rpc/services
   {:users
    {:url "http://ccc.com/aaa"
     :auth ["fff" "ggg"]}
    :sales
    {:url "http://bbb.com/ccc"
     :auth ["fff" "ggg"]}}})


(defn generate-id [fn-id]
  (cond
    (= fn-id :int)
    (rand-int 99999)

    (= fn-id :uuid)
    (str (java.util.UUID/randomUUID))

    (fn? fn-id)
    (fn-id)

    :else
    (throw (ex-info "Wrong ID function" {:fn-id fn-id}))))


(defn rpc-inner

  ([config method]
   (rpc-inner config method nil))

  ([config method params]

   (let [config
         (merge config config-defaults)

         {:rpc/keys
          [multi?
           services
           fn-dispatch-service]} config

         config-service
         (when multi?
           (or (some-> method
                       fn-dispatch-service
                       (get services))
               (throw (ex-info "RPC service not found"
                               {:method method}))))

         config-method
         (get-in config [:rpc/method-options method])

         config
         (merge config
                config-service
                config-method)

         {:rpc/keys
          [notify?
           fn-id
           fn-before-send]} config

         id
         (when-not notify?
           (generate-id fn-id))

         payload
         (cond-> {:version "2.0"
                  :method method}
           id
           (assoc :id id)
           params
           (assoc :params params))

         request
         (-> config
             (assoc :form-params payload))]

     (-> request
         fn-before-send
         client/request
         :body))))


(defn call

  ([config method]
   (rpc-inner config method))

  ([config method params]
   (if (map? params)
     (rpc-inner config method params)
     (rpc-inner config method [params])))

  ([config method arg & args]
   (rpc-inner config method (cons arg args))))


(defn with-notify [config method]
  (assoc-in
   config
   [:rpc/method-options method :rpc/notify?]
   true))


(defn notify

  ([config method]
   (call (with-notify config method) method))

  ([config method params]
   (call (with-notify config method) method params))

  ([config method arg & args]
   (apply call (with-notify config method) method arg args)))


(defn batch [client batches]

  )


#_
(batch config
       [:user/get-by-id 1]
       [:user/get-by-id 2]
       [:user/get-by-id 3])



#_
(defn remap-batch
  [[method arg & args]]

  (let [params 1])


  (if (map? arg)
    (if args
      (throw (new Exception "Wrong params"))
      {:params arg})
    {:params (into [arg] args)}))


#_
(defn remap-batches
  [batches]
  (mapv remap-batch batches))



(defn call-batch
  [config batches]

  (rpc-inner config (remap-batches batches))






  )



#_
(rpc config :user/get-by-id 42)

#_
(rpc client :job/poll "34234235235")

#_
(rpc client :users/get-by-id 42)

#_
(rpc-client :users/get-by-id {:id 42})



#_
(rpc-batch client
           [[:users/get-by-id 42]
            [:users/create {:name "Ivan"}]] )

#_
(defn make-multi-client [opt]
  )



#_
(rpc-call config :foo/bar 1 2 3)

#_
(rpc-call config :foo/bar {:name "Ivan"
                           :age 42
                           :email "foo@test.com"})

#_
(rpc-notify config :foo/bar 1 2 3)

#_
(rpc-call config ^:notify [:foo/bar 1 2 3])


#_
(rpc-batch config

            [:foo/bar 1]
           ^:notify [:foo/bar 2]
           [:foo/bar 3]
           )



#_
(def multi-client

  {:dispatch
   (comp keyword namespace)

   :services
   {:users
    {:endpoint "http://test.com/test"
     :auth ["user" "pass"]
     :headers {:foo 42}
     :timeout 30000
     :throw-exceptions? 1}

    :sales
    {:endpoint "http://test.com/test"
     :auth ["user" "pass"]
     :headers {:foo 42}
     :timeout 30000
     :throw-exceptions? 1}}
   }

  {:users 1
   }


  )

#_
(rpc multi-client :users/get-user-by-id 42)
