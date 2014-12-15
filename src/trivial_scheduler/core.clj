(ns trivial-scheduler.core
  (:refer-clojure :exclude [compile])
  (:require
    [clj-time.core :as t]))

(defn- range-for
  "Return the range of values for a given tab field."
  [field]
  (-> (case field
        :minute (range 0 60)
        :hour (range 0 24)
        :month (range 1 13)
        :date (range 1 32)
        :day (range 1 8)
        :task [:noop]
        nil)
    (vec)))

(defn- periodic
  "Return a range of values representing every 'n'th value for a given
  'field'."
  [field n]
  (vec (for [x (range-for field) :when (= 0 (mod x n))] x)))

(def ^:private default-tab
  "By default, a tab will run every minute."
  (let [ks [:minute :hour :month :date :day :task]]
    (into {} (map #(vector % (range-for %)) ks))))

(defn- expand-periodic
  "Expand [:periodic n] declarations to their range values."
  [tab]
  (letfn [(expander [field [xform val :as spec]]
            (if (= :periodic xform)
              (periodic field val)
              spec))]
    (reduce (fn [a [k v]]
              (assoc a k (expander k v))) {} tab)))

(defn- expand
  "Expand a declarative 'tab' representation into actual data."
  [tab]
  (->> (select-keys tab (keys default-tab))
    expand-periodic
    (merge default-tab)))

(defn- ticker
  "Return a 'tab' representing the 'time'."
  [time]
  {:minute (t/minute time)
   :hour (t/hour time)
   :day (t/day-of-week time)
   :month (t/month time)
   :date  (t/day time)})

(defn- match?
  "True if the 'tick' tab falls within the ranges in 'tab'."
  [tick tab]
  (empty? (filter #(not (contains? (set (% tab)) (% tick))) (keys tick))))

;;-----------------------------------------------------------------------------
;; Public

(defn tasks-to-run
  "Return a list of the tasks to be run at the given time."
  [crontab time]
  (let [tick (ticker time)]
    (mapv :task (filter #(match? tick %) crontab))))

(defn compile
  "Turn a spec into a queryable object."
  [specs]
  (mapv expand specs))
