(ns jsonrpc.server
  (:require
   [jsonrpc.spec :as spec]

   [clojure.tools.logging :as log]

   [clojure.spec.alpha :as s]))


;; check specs in config
;; check functions in config
;; auto-load func ns in config

;; refactor errors
;; better explain (expound?)
;; drop version


(def rpc-errors
  {:parse-error
   {:status -32700
    :code 500
    :message "Parse error"}

   :invalid-request
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
  [params]

  (let [{:keys [type]} params

        context
        (or (get rpc-errors type)
            (get rpc-errors :internal-error))

        data
        (merge context params)]

    (throw (ex-info "RPC error" data))))


(defn process-rpc-single
  [config rpc request]

  (let [{:keys [id method params version]}
        rpc

        {:keys [conform-in-spec?
                validate-out-spec?]}
        config

        [_ id] id

        notification? (some? id)]

    (try

      (let [method (keyword method)

            rpc-map
            (get-in config [:methods method])

            _
            (when-not rpc-map
              (rpc-error! {:type :not-found
                           :data {:method method}}))

            {:keys [spec-in
                    spec-out
                    handler]} rpc-map

            params
            (if (and conform-in-spec? spec-in)
              (s/conform spec-in params)
              params)

            _
            (when (s/invalid? params)
              (let [explain (s/explain-str spec-in params)]
                (rpc-error! {:type :invalid-params
                             :data {:method method
                                    :explain explain}})))

            result
            (try
              (handler params request)
              (catch Throwable e
                (log/errorf e "Handler error, id: %s, method: %s, params: %s"
                            id method params)
                (rpc-error! {:type :internal-error
                             :data {:method method}})))

            _
            (when (and validate-out-spec? spec-out)
              (when-not (s/valid? spec-out result)
                (let [explain (s/explain-str spec-out result)]
                  (log/errorf "Invalid out spec, id: %s, method: %s, explain: %s"
                              id method explain))
                (rpc-error! {:type :internal-error
                             :data {:method method}})))]

        (when-not notification?
          {:id id
           :version version
           :result result}))

      (catch Throwable e
        (let [{:keys [type data message code]}
              (ex-data e)]
          {:id id
           :version version
           :error {:code code
                   :message message
                   :data data}})))))


(defn guess-http-status [response]
  (if (:error response)
    500
    200))


(defn process-rpc-batch
  [config rpc-list request]

  (let [{:keys [parallel-batch?
                max-batch-size]} config

        exceeded?
        (when max-batch-size
          (> (count rpc-list) max-batch-size))

        _
        (when exceeded?
          (rpc-error! {:type :invalid-params
                       :message "Batch size is too large"}))

        fn-map
        (if parallel-batch? pmap map)

        fn-single
        (fn [rpc]
          (process-rpc-single config rpc request))]

    (fn-map fn-single rpc-list)))



#_
{:data-field :params
 :conform-in-spec? true
 :validate-out-spec? true
 :allow-batch? true
 :max-batch-size 20
 :parallel-batch? false
 :methods
 {:users/get-by-id
  {:title "Get a user by ID"
   :description "Some long description of the method"
   :handler 'project.handler.user/get-by-id
   :spec-in :user/get-by-id.in
   :spec-out :user/get-by-id.out}
  :users/create-user
  {:title "Get a user by ID"
   :description "Some long description of the method"
   :handler 'project.handler.user/create
   :spec-in :user/create.in
   :spec-out :user/create.out}}}

(def config-default
  {:data-field :params
   :conform-in-spec? true
   :validate-out-spec? true
   :allow-batch? true
   :max-batch-size 25
   :parallel-batch? true})


(defn make-handler
  [config]

  (let [config
        (merge config-default config)

        {:keys [data-field
                allow-batch?]} config]

    (fn [request]

      (try

        (let [payload
              (get request data-field)

              parsed
              (s/conform ::spec/rpc payload)

              _
              (when (s/invalid? parsed)
                (rpc-error! {:type :invalid-request}))

              [tag rpc] parsed]

          (when (and (= tag :batch)
                     (not allow-batch?))
            (rpc-error! {:type :invalid-params
                         :message "Batch requests are not allowed"}))

          (case tag
            :single
            (let [response (process-rpc-single config rpc request)
                  status (guess-http-status response)]
              {:status status
               :body response})

            :batch
            (let [response (process-rpc-batch config rpc request)
                  response (remove nil? response)]
              {:status 200
               :body response})))

        (catch Throwable e
          ;; (log/error e ...)
          {:status 500
           :body {:error {:code -32600
                          :message "sdfsfds"}}})))))


;;;;;;;;;



#_
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


#_
(defn prepare-rpc-mapping
  [method->rpc-map]
  (into {} (for [[k v] method->rpc-map]
             [k (update v :handler ensure-fn)])))
