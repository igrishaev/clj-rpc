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
   {:status 500
    :code -32700
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


(def into-map (partial into {}))


(def code->status
  (into-map
   (map (juxt :code :status)
        (vals rpc-errors))))


(defn rpc-error!
  [params]

  (let [{:keys [type]} params

        rpc-error
        (or (get rpc-errors type)
            (get rpc-errors :internal-error))

        data
        (merge rpc-error params)]

    (throw (ex-info "RPC error" data))))


(defn find-method
  [{:as this :keys [config rpc-parsed]}]

  (let [{:keys [id method]} rpc-parsed
        {:keys [methods]} config

        rpc-map (get methods method)]

    (if-not rpc-map
      (rpc-error! {:id id
                   :type :not-found
                   :data {:method method}})
      (assoc this
             :rpc-map rpc-map))))


(defn validate-params
  [{:as this :keys [config rpc-map rpc-parsed]}]

  (let [{:keys [id method]} rpc-parsed

        {:keys [conform-in-spec?]}
        config

        {:keys [params]} rpc-parsed

        [_ params] params

        {:keys [spec-in
                spec-out
                handler]} rpc-map

        params-parsed
        (if (and conform-in-spec? spec-in)
          (s/conform spec-in params)
          params)]

    (if (s/invalid? params-parsed)

      (let [explain (s/explain-str spec-in params)]
        (rpc-error! {:id id
                     :type :invalid-params
                     :data {:method method
                            :explain explain}}))

      (assoc this
             :handler handler
             :params params-parsed))))


(defn execute-method
  [{:as this :keys [handler params request]}]
  (let [rpc-result
        (handler params request)]
    (assoc this :rpc-result rpc-result)))


(defn validate-output
  [{:as this :keys [config rpc-result rpc-map rpc-parsed]}]

  (let [{:keys [spec-out]} rpc-map

        {:keys [validate-out-spec?]} config

        {:keys [method]} rpc-parsed

        result
        (when (and validate-out-spec? spec-out)
          (s/valid? spec-out rpc-result))]

    (if (s/invalid? result)

      (let [explain (s/explain-str spec-out rpc-result)]
        (rpc-error! {:type :internal-error
                     :data {:method method}}))

      this)))


(defn compose-response
  [{:as this :keys [rpc-parsed rpc-result]}]
  (let [{:keys [id version]} rpc-parsed]
    (when id
      {:id id
       :version version
       :result rpc-result})))


(def types-no-log
  #{:invalid-request
    :not-found
    :invalid-params})


(defn rpc-single-error-handler
  [e]
  (let [{:keys [id code message data]}
        (ex-data e)]

    (when-not (contains? types-no-log type)
      (log/error e))

    {:id id
     :jsonrpc "2.0"
     :error {:code code
             :message message
             :data data}}))


(defn process-rpc-single
  [this]
  (-> this
      find-method
      validate-params
      execute-method
      validate-output
      compose-response
      (try
        (catch Throwable e
          (rpc-single-error-handler e)))))


(defn guess-http-status
  [{:keys [error]}]
  (if error
    (let [{:keys [code]} error]
      (get code->status code 500))
    200))


(defn check-batch-limit
  [{:as this :keys [config rpc-parsed]}]

  (let [{:keys [max-batch-size]} config
        batch-size (count rpc-parsed)

        exeeded?
        (when max-batch-size
          (> batch-size max-batch-size))]

    (if exeeded?
      (rpc-error! {:type :invalid-params
                   :message "Batch size is too large"})
      this)))


(defn execute-batch
  [{:as this :keys [config rpc-parsed]}]

  (let [{:keys [parallel-batch?]} config

        fn-map
        (if parallel-batch? pmap map)

        fn-single
        (fn [rpc-single]
          (process-rpc-single
             (assoc this :rpc-parsed rpc-single)))]

    (fn-map fn-single rpc-parsed)))


(defn process-rpc-batch
  [this]
  (-> this
      check-batch-limit
      execute-batch))


(defn step-1-parse-payload
  [{:as this :keys [config request]}]

  (let [{:keys [data-field]} config

        payload
        (get request data-field)

        rpc-parsed
        (s/conform ::spec/rpc payload)]

    (if (s/invalid? rpc-parsed)
      (let [explain (s/explain-str ::spec/rpc payload)]
        (rpc-error! {:type :invalid-request
                     :data {:explain explain}}))

      (let [[tag rpc-parsed] rpc-parsed
            batch? (= tag :batch)]
        (assoc this
               :batch? batch?
               :rpc-parsed rpc-parsed)))))


(defn step-2-check-batch
  [{:as this :keys [config batch?]}]

  (let [{:keys [allow-batch?]} config]

    (if (and batch? (not allow-batch?))
      (rpc-error! {:type :invalid-request})
      this)))


(defn step-3-process-rpc
  [{:as this :keys [batch? rpc-parsed]}]

  (let [rpc-result
        (if batch?
          (process-rpc-batch this)
          (process-rpc-single this))]

    (assoc this :rpc-result rpc-result)))


(defn step-4-http-response
  [{:as this :keys [batch? rpc-result]}]

  (if batch?

    (let [rpc-result (remove nil? rpc-result)]
      {:status 200
       :body rpc-result})

    (let [status (guess-http-status rpc-result)]
      {:status status
       :body rpc-result})))


(def config-default
  {:data-field :params
   :conform-in-spec? true
   :validate-out-spec? true
   :allow-batch? true
   :max-batch-size 25
   :parallel-batch? true})


(defn make-handler
  [config]
  (fn [request]

    (-> {:config (merge config-default config)
         :request request}

        step-1-parse-payload
        step-2-check-batch
        step-3-process-rpc
        step-4-http-response

        (try
          (catch Throwable e
            (log/error e)

            (let [{:keys [status code message data]}
                  (ex-data e)]

              {:status (or status 500)
               :body {:error {:code code
                              :message message
                              :data data}}}))))))



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


#_
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
