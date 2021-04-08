(ns jsonrpc.schema
  (:require
   [clojure.string :as str]
   [clojure.spec.alpha :as s]))


(declare make-schema)


;; (alias 'c 'clojure.core)

(defmulti pred->schema identity)


(defmethod pred->schema :default [_])


(defmacro defpred [symbol schema]
  `(defmethod pred->schema '~symbol [~'_]
     ~schema))


(defpred clojure.core/boolean?
  {:type :boolean})

(defpred clojure.core/string?
  {:type :string})

(defpred clojure.core/int?
  {:type :integer})

(defpred clojure.core/pos-int?
  {:type :integer
   :minimum 0})

(defpred clojure.core/nat-int?
  {:type :integer
   :minimum 1})

(defpred clojure.core/inst?
  {:type :date-time})

(defpred clojure.core/nil?
  {:type :null})

(defpred clojure.core/uuid?
  {:type :string
   :pattern "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$"})

(defpred clojure.core/float?
  {:type :number})

(defn cons? [x]
  (instance? clojure.lang.Cons x))

(defmulti expr->schema first)

(defmethod expr->schema :default [_])

(defmacro defexpr [dispatch bindings  & body]
  `(defmethod expr->schema ~dispatch
     [~bindings]
     ~@body))


(defexpr `s/keys
  [_ & {:keys [req req-un opt opt-un]}]

  {:type :object
   :properties
   (into {} (for [spec req-un]
              [(name spec) (make-schema spec)]))

   :required
   (for [spec req-un]
     (name spec))})


(defexpr `s/coll-of
  [_ spec-item & {:keys [count
                         distinct
                         min-count
                         max-count]}]

  (cond-> {:type :array
           :items (make-schema spec-item)}

    distinct
    (assoc :uniqueItems true)

    min-count
    (assoc :minItems min-count)

    max-count
    (assoc :maxItems max-count)))



(defexpr `s/tuple
  [_ & specs]
  {:type :array
   :items (mapv make-schema specs)})


(defn form->schema [form]
  (cond
    (symbol? form) (pred->schema form)
    (cons? form) (expr->schema form)))


(defmulti spec->schema identity)


(defmethod spec->schema :default [_] nil)


(defmacro defspec [spec schema]
  `(defmethod spec->schema ~spec [~'_]
     ~schema))


(def default-schema {:type :any})


(defmulti guess-type type)


(defmethod guess-type String [_]
  "string")

(defmethod guess-type Integer [_]
  "integer")

(defmethod guess-type Long [_]
  "integer")


(defn enum->schema [spec]
  (if (= 1 (count spec))
    {:const (first spec)}
    {:enum (into [] spec)}))


(defn make-schema [spec]
  (merge
   default-schema

   (cond

     (set? spec)
     (enum->schema spec)

     (cons? spec)
     (expr->schema spec)

     (qualified-symbol? spec)
     (pred->schema spec)

     (qualified-keyword? spec)
     (merge
      (when-let [form (some-> spec s/get-spec s/form)]
        (make-schema form))
      (spec->schema spec)))))


;;

(s/def ::age int?)

(s/def ::address string?)

(s/def ::addresses (s/coll-of ::address :min-count 2 :max-count 5 :distinct true))

(s/def ::user
  (s/keys :req-un [::age
                   ::addresses
                   ::bbbb]))

(defspec ::age
  {:title "Age"
   :description "User age in years"})


(s/def ::xxx (s/tuple int? int? keyword?))


(s/def ::bbb #{"sdfsd" "ffff" "sdfsdf"})
