(ns jsonrpc.test
  (:require
   [clojure.string :as str]

   ;; [clojure.instant :as instant]
   ;; [clojure.spec.alpha :as s]

   ))



(defmacro with-mock
  [[params mapping] & body]


  `(let [server# {}]

     (try
       ~@body
       (finally
         (.close server#))))

  )



(with-mock [{:port 9999 :path "/api"}
            {:get-user-by-id
             {:status 200
              :body {:foo 42}}
             :list-users
             {:status 200
              :body [{:foo 42} {:foo 42}]}

             }
            ]



  )


(with-mock
  {:port 9999
   :path "/api"
   :method->response
   {:get-user-by-id
    {:status 200
     :body {:foo 42}}

    :list-users
    {:status 200
     :body [{:foo 42} {:foo 42}]}

    :delete-user
    {:status 200
     :body [{:foo 42} {:foo 42}]}}}

  (println 42))
