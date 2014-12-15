(ns trivial-scheduler.service
  "A simple example of a background scheduler service publishing
   scheduled events to a channel."
  (:require
    [clojure.core.async :refer [go put! timeout]]
    [trivial-scheduler.core :as ts]))

(defn publish!
  [tasks output]
  (try
    (doseq [t tasks]
      (put! output t))
    true
    (catch Throwable _
      false)))

(defn forever!
  [{:keys [spec output] :as service} control]
  (go (loop []
        (let [[v ch] (alts! [control (timeout 1000)])]
          (if (and (= ch control) (nil? v))
            :done
            (when (publish! (ts/tasks-to-run spec) output)
              (recur)))))))

(defn mk-service
  [spec ch]
  {:spec (ts/compile spec)
   :output ch})

(defn start!
  [service]
  )

(defn stop!
  [service]
  )
