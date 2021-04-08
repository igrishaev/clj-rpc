(ns jsonrpc.server
  (:require
   [jsonrpc.spec :as spec]
   [clojure.spec.alpha :as s]))


(def rpc-errors
  {:invalid-request
   {:status 400
    :code -32600
    :message "Invalid Request"}

   :not-found
   {:status 404
    :code -32601
    :message "Method not found"}

   :invalid-params
   {:status 400
    :code -32602
    :message "Invalid params"}

   :internal-error
   {:status 500
    :code -32603
    :message "Internal error"}})


(defn rpc-error!

  ([]
   (rpc-error! :internal-error))

  ([type]
   (rpc-error! type nil))

  ([type params]
   (let [context (or (get rpc-errors type)
                     (get rpc-errors :internal-error))

         data (-> context
                  (merge params)
                  (assoc :type :rpc))]

     (throw (ex-info "RPC error" data)))))


(def opt-default
  {:data-field :params
   :validate-spec-out? true})


(defn ensure-fn [obj]
  (cond

    (fn? obj)
    obj

    (and (var? obj) (fn? @obj))
    obj

    (and (symbol? obj) (resolve obj))
    (resolve obj)

    :else
    (throw (new Exception (format "Wrong function: %s" obj)))))


(defn prepare-rpc-mapping
  [method->rpc-map]
  (into {} (for [[k v] method->rpc-map]
             [k (update v :handler ensure-fn)])))


(defn make-handler
  [opt method->rpc-map]

  (let [opt* (merge opt-default opt)

        {:keys [data-field
                validate-spec-out?]} opt*

        method->rpc-map*
        (prepare-rpc-mapping method->rpc-map)]

    (fn [request]

      (let [payload
            (get request data-field)

            id
            (get payload :id)

            version
            (get payload :version)

            notification? (some? id)]

        (try

          (when-not (s/valid? ::spec/rpc payload)
            (rpc-error! :invalid-request))

          (let [{:keys [method params]}
                payload

                method (keyword method)

                rpc-map
                (get method->rpc-map* method)

                _
                (when-not rpc-map
                  (rpc-error! :not-found {:data {:method method}}))

                {:keys [spec-in
                        spec-out
                        handler]} rpc-map

                params*
                (if spec-in
                  (s/conform spec-in params)
                  params)

                _
                (when (s/invalid? params*)
                  (rpc-error! :invalid-params))

                result
                (handler params* request)

                _
                (when (and spec-out validate-spec-out?)
                  (when-not (s/valid? spec-out result)
                    (rpc-error! :internal-error {:data (s/explain-data spec-out result)})))]

            {:status 200
             :body {:id id
                    :version version
                    :result result}})

          (catch Exception e
            (println e)
            (let [{:keys [type
                          code
                          data
                          message
                          status]} (ex-data e)]

              (if (= type :rpc)

                {:status status
                 :body (when-not notification?
                         {:id id
                          :version version
                          :error {:code code
                                  :message message
                                  :data data}})}

                (throw e)))))))))


(defn user-get-by-id [params])


(defn user-create [params])


;; todo support fdef


;; handler symbol


(def method->rpc-map
  {:users/get-by-id
   {:description "Get a user by ID"
    :spec-in :user/get-by-id
    :spec-out :user/user
    :handler #'user-get-by-id}

   :users/create
   {:description "Create a user"
    :spec-in :user/create
    :spec-out :user/user
    :handler #'user-create}})
