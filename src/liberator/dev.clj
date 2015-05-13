(ns liberator.dev
  (:use compojure.core
        [liberator.core :only [defresource]]
        [clojure.string :only [join]])
  (:require [liberator.core :as core]
            [clojure.string :as string]
            [clojure.data.json :as json])
  (:import java.util.Date))

(defonce logs (atom nil))

(defn next-id [] (apply str (take 5 (repeatedly
                                     #(rand-nth "abcdefghijklmnopqrstuvwzxy0123456789")))))

(def log-size 100)

(defn save-log! [id msg]
  (swap! logs #(->> (conj % [id msg])
                    (take log-size))))

(defn- with-slash [^String s] (if (.endsWith s "/") s (str s "/")))

(def ^:dynamic *current-id* nil)

(defn log-by-id [id]
  (first (filter (fn [[aid _]] (= id aid)) @logs)))

(def trace-id-header "X-Liberator-Trace-Id")

(defn- wrap-trace-header [handler]
  (fn [req]
    (let [resp (handler req)]
      (if-let [id (get-in resp [:headers trace-id-header])]
        (let [[_ [_ _ l]] (log-by-id id)]
          (assoc-in resp [:headers "X-Liberator-Trace"]
                    (map #(clojure.string/join " " %) l)))
        resp))))

(defn- cond-wrap [fn expr wrapper]
  (if expr (wrapper fn) fn))

(defn- wrap-decision-logging [handler]
  (fn [request]
    (let [request-log (atom [])]
      (binding [*current-id* (next-id)]
        (core/with-logger (core/atom-logger request-log)
          (let [resp (handler request)]
            (if-not (empty? @request-log)
              (do
                (save-log! *current-id*
                           [(Date.)
                            (select-keys request [:request-method :uri :headers :params])
                            @request-log])
                (assoc-in resp [:headers trace-id-header] *current-id*))
              resp)))))))

(defn wrap-trace
  "Wraps a ring handler such that a request trace is generated."
  [handler]
  (-> handler
      wrap-decision-logging
      wrap-trace-header))
