(ns lambdaisland.webstuff.bootstrap
  "Plumbing for boostrapping an Integrant based app with an Aero config file"
  (:require [aero.core :as aero]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [integrant.core :as ig]
            [integrant.repl :as ig-repl]
            [lambdaisland.glogc :as log]))

(defmethod aero/reader 'ig/ref [_ _tag value] (ig/ref value))
(defmethod aero/reader 'ig/refset [_ _tag value] (ig/refset value))

(def config-file
  "Resource (classpath-based) path to an Aero/Integrant EDN configuration

  Change with [[set-config!]]"
  "config.edn")

(defn set-config!
  "Set the path of the EDN config file, to be looked up on the classpath

  By convention this should be called `\"<application_name>/config.edn\"`, and
  be stored under `resources/<application_name>/config.edn`. e.g. `(set-config!
  \"todo/config.edn\")` "
  [resource-path]
  (alter-var-root #'config-file (constantly resource-path)))

(defn ig-config
  "Get the integrant config map"
  ([]
   (ig-config :default))
  ([profile]
   (aero/read-config (io/resource config-file) {:profile profile})))

(defn set-prep!
  "Register out prep function with integrant-repl

  The prep-function will load the config, using the given profile, and let
  Integrant load any referenced namespaces."
  ([]
   (set-prep! :default))
  ([profile]
   (ig-repl/set-prep! #(doto (ig-config profile) ig/load-namespaces))))

(defn- add-shutdown-hook [f]
  (.addShutdownHook (java.lang.Runtime/getRuntime) (Thread. f)))

(defn go
  "Start the integrant system

  By default starts everything, pass a key or collection of keys to only start
  those keys. Registers a shutdown hook so stop handlers run when the
  application halts."
  ([]
   (go nil))
  ([{:keys [profile key]
     :or {profile :default}}]
   (log/info :integrant/starting {:profile profile :key key})
   (set-prep! profile)
   (add-shutdown-hook
    (fn []
      (ig-repl/halt)
      (log/info :shutdown/finished {})))
   (cond
     (coll? key)
     (ig-repl/go key)
     (keyword? key)
     (ig-repl/go [key])
     (nil? key)
     (ig-repl/go))))
