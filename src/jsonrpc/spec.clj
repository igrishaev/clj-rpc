(ns jsonrpc.spec
  (:require
   [clojure.string :as str]
   [clojure.instant :as instant]
   [clojure.spec.alpha :as s]))


(s/def ::ne-string
  (s/and string? (complement str/blank?)))


(s/def ::id (s/or :string string? :int int?))

(s/def ::version #{"2.0"})

(s/def ::method ::ne-string)

(s/def ::params-map
  (s/map-of keyword? any?))

(s/def ::params
  (s/or :map ::params-map :vec vector?))

(s/def ::rpc
  (s/keys :req-un [::version
                   ::method
                   ::params]
          :opt-un [::id]))


(defmacro with-conformer
  [[bind] & body]
  `(s/conformer
    (fn [~bind]
      (try
        ~@body
        (catch Exception e#
          ::s/invalid)))))


(s/def ::->int
  (with-conformer [val]
    (cond
      (int? val) val
      (string? val) (Integer/parseInt val)
      :else ::s/invalid)))


(s/def ::->long
  (with-conformer [val]
    (cond
      (int? val) val
      (string? val) (Long/parseLong val)
      :else ::s/invalid)))


(s/def ::->float
  (with-conformer [val]
    (cond
      (float? val) val
      (string? val) (Float/parseFloat val)
      :else ::s/invalid)))


(s/def ::->double
  (with-conformer [val]
    (cond
      (double? val) val
      (string? val) (Double/parseDouble val)
      :else ::s/invalid)))


(s/def ::->date
  (with-conformer [val]
    (cond
      (inst? val) val
      (string? val) (instant/read-instant-date val)
      :else ::s/invalid)))


(s/def ::->bool
  (with-conformer [val]
    (cond
      (boolean? val) val
      (nil? val) false
      (string? val)
      (case (str/lower-case val)
        ("true" "1") true
        ("false" "0" "") true)
      :else ::s/invalid)))
