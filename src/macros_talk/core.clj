(ns macros-talk.core
  (:require [clojure.tools.namespace.repl :refer :all])
  (:gen-class))

(declare unless)

;(unless (= 2 3) 
;        (println "Woah, the universe is ending!")
;        (println "All is fine"))

(defn unless [test-exp false-exp true-exp]
  (if test-exp false-exp true-exp))
















;macros-talk.core=> (unless (= 2 3) 
;              #_=>     (println "Woah, the universe is ending!")
;              #_=>     (println "All is fine"))
;Woah, the universe is ending!
;All is fine
;nil

(defn unless [test-exp false-exp true-exp]
  (if test-exp (false-exp) (true-exp)))






















;macros-talk.core=> (unless (= 2 3) 
;              #_=>         (fn [] (println "Woah, the universe is ending!"))
;              #_=>         (fn [] (println "All is fine")))
;All is fine
;nil

(defmacro unless [test-form false-form true-form]
  `(if ~test-form ~false-form ~true-form))



























;(unless (= 2 3) 
;        (println "Woah, the universe is ending!")
;        (println "All is fine"))
;All is fine
;nil






























(-> 2 inc inc (* 2))

;macros-talk.core=> (source ->)
;(defmacro ->
;  "Threads the expr through the forms. Inserts x as the
;  second item in the first form, making a list of it if it is not a
;  list already. If there are more forms, inserts the first form as the
;  second item in second form, etc."
;  {:added "1.0"}
;  ([x] x)
;  ([x form] (if (seq? form)
;              (with-meta `(~(first form) ~x ~@(next form)) (meta form))
;              (list form x)))
;  ([x form & more] `(-> (-> ~x ~form) ~@more)))

; macros-talk.core=> (macroexpand-1 '(-> 2 inc))
; (inc 2)

;macros-talk.core=> (macroexpand-1 '(-> 2 inc inc))
;(clojure.core/-> (clojure.core/-> 2 inc) inc)

;macros-talk.core=> (macroexpand-1 (macroexpand-1 '(-> 2 inc inc)))
;(inc (clojure.core/-> 2 inc))
;
;macros-talk.core=> (use '[clojure.walk :refer [macroexpand-all]])
;
;macros-talk.core=> (macroexpand-all '(-> 2 inc inc))
;(inc (inc 2))
;
;
;
;macros-talk.core=> (macroexpand-all '(-> 2 inc inc (* 2)))
;(* (inc (inc 2)) 2)
;






























(defn foo [] (println "foo"))

(defmacro call-foo []
  '(foo))





;bar=> (ns baz)
;nil
;baz=> (require '[macros-talk.core :refer [call-foo]])
;nil
;baz=> (call-foo)
;
;CompilerException java.lang.RuntimeException: Unable to resolve symbol: foo in this context, compiling:(NO_SOURCE_PATH:64:4) 














(defmacro call-foo []
  `(foo))

;baz=> (ns baz)
;nil
;baz=> (require '[macros-talk.core :refer [call-foo]])
;nil
;baz=> (call-foo)
;foo
;nil

















(facts "on fetching customer -> device and device -> customer"
       (let [device              (gen-device)
             customer            (gen-customer :device (:db/id device))
             {:keys [tempids]}   (add-bulk [device customer])
             device-eid          (resolve-tempid tempids (:db/id device))
             customer-eid        (resolve-tempid tempids (:db/id customer))]

         (fact "given a device eid, we receive a customer"
               (device-eid->customer-eid device-eid) => customer-eid)

         (fact "given a customer eid, we receive the primary device"
               (customer-eid->one-device-eid customer-eid) => device-eid)))







(facts "on fetching customer -> device and device -> customer"
  (db-let [device              (gen-device)
           customer            (gen-customer :device (:db/id device))]
          (fact "given a device eid, we receive a customer"
                (device-eid->customer-eid (eid device)) => (eid customer))
          (fact "given a customer eid, we receive the primary device"
                (customer-eid->one-device-eid (eid customer)) => (eid device))))



(defmacro db-let
  "let-like macro to save (using add-bulk) all binding values,
   Permanent eids for the saved entities are available with (eid value)"
  [bindings & body]
  (let [bulk-results-sym (gensym "bulk-results-sym")]
  `(let ~bindings
     (let [~bulk-results-sym (trans/add-bulk [~@(to-save bindings)])]
       (let [~@(enrich-with-eid bindings bulk-results-sym)]
         ~@body)))))

(defn enrich-with-eid [bindings bulk-results-sym]
  (let [definitions (partition 2 bindings)]
    (->> definitions
      (map (fn [[k v]] [k `(append-eid ~k ~bulk-results-sym)])) (apply concat))))

(defn append-eid [e bulk-results]
  (let [eid (dc/resolve-tempid (:tempids bulk-results) (:db/id e))]
  (with-meta e {:eid eid})))

(defn eid
  "Extracts the eid from entities created with db-let"
  [entity]
  (->> (meta entity) :eid))
