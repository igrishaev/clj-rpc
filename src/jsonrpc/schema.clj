(ns jsonrpc.schema
  (:require
   [clojure.string :as str]
   [clojure.spec.alpha :as s]))


;; (alias 'c 'clojure.core)


(defmulti pred->schema identity)


(defmethod pred->schema :default [_]
  nil)


(defmacro defpred [symbol schema]
  `(defmethod pred->schema '~symbol [~'_]
     ~schema))


(defpred clojure.core/boolean?
  {:type :boolean})

(defpred clojure.core/string?
  {:type :string})

(defpred clojure.core/pos-int?
  {:type :integer
   :minimum 0})

(defpred clojure.core/nat-int?
  {:type :integer
   :minimum 1})

(defpred clojure.core/inst?
  {:type :date-time})

(defpred clojure.core/uuid?
  {:type :string
   :pattern "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$"})

(defpred clojure.core/float?
  {:type :number})


(defn cons? [x]
  (instance? clojure.lang.Cons x))


(defmulti expr->schema first)

(defmethod expr->schema :default [_])


(defmacro defexpr [init [form] & body]
  `(defmethod expr->schema ~init
     [~form]
     ~@body))


(defexpr `s/keys
  [form]
  (let [mapping (apply hash-map (rest form))

        {:keys [req
                req-un
                opt
                opt-un]} mapping]

    ;;
    {:type :object
     :properties
     (into {} (for [spec req-un]
                [(name spec) (make-schema spec)]))

     :required
     (for [spec req-un]
       (name spec))}))


(defexpr `s/coll-of
  [form]
  (let [[_ spec-item & args] form
        arg-map (apply hash-map args)
        {:keys [count
                min-count
                max-count]} arg-map]
    ;;
    {:type :array
     :items (make-schema spec-item)}))


(defn form->schema [form]
  (cond
    (symbol? form) (pred->schema form)
    (cons? form) (expr->schema form)))


(defmulti spec->schema identity)


(defmethod spec->schema :default [spec]
  nil)


(defmacro defspec [spec schema]
  `(defmethod spec->schema ~spec [~'_]
     ~schema))


(defn make-schema [spec]
  (merge
   {:type :any}
   (form->schema (some-> spec s/get-spec s/form))
   (spec->schema spec)))


;;

(s/def ::age int?)

(s/def ::address string?)

(s/def ::addresses (s/coll-of ::address))

(s/def ::user
  (s/keys :req-un [::age
                   ::addresses]))

(defspec ::age
  {:title "Age"
   :description "User age in years"})

(defspec ::user
  {:title "User"})
