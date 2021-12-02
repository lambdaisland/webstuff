(ns lambdaisland.webstuff.http
  "Provide all the building blocks for setting up Jetty/Sieppari/Reitit

  In particular this provides a setup where routes can easily use content
  negotiation to return either data (as EDN/JSON/Transit/etc), or to run that
  same data through a view function (data->hiccup) to render HTML.

  In this case the `:body` in the response is always a data structure, and if
  the body is able to return HTML then the response should also include a
  `:view` function for rendering."
  (:require [clojure.pprint :as pprint]
            [lambdaisland.glogc :as log]
            [lambdaisland.hiccup :as hiccup]
            [muuntaja.core :as m]
            [muuntaja.format.core :as muuntaja-format]
            [muuntaja.interceptor :as muuntaja-interceptor]
            [reitit.http :as http]
            [reitit.http.coercion :as coercion]
            [reitit.http.interceptors.exception :as exception]
            [reitit.http.interceptors.multipart :as multipart]
            [reitit.http.interceptors.muuntaja :as muuntaja]
            [reitit.http.interceptors.parameters :as parameters]
            [reitit.interceptor.sieppari :as sieppari]
            [reitit.middleware :as middleware]
            [reitit.ring :as ring]
            [ring.adapter.jetty :as jetty])
  (:import (java.io OutputStream)
           (org.eclipse.jetty.server Server)))

(defn- terminate
  "Terminate the interceptor chain

  An odly missing piece in Sieppari's API, other interceptor implementations
  have this out of the box."
  [ctx response]
  (-> ctx
      (dissoc :queue)
      (assoc :response response)))

(defn html-encoder
  "Muuntaja encoder that renders HTML

  Expects a Clojure collection with a `:view-fn` in the metadata, which takes
  the `:body` collection as argument, and returns the body as a string."
  [opts]
  (reify muuntaja-format/EncodeToBytes
    (encode-to-bytes [_ data charset]
      (let [view (get (meta data) :view-fn)
            rendered (cond
                       (ifn? view)
                       (view data)

                       (string? data)
                       data

                       :else
                       (pr-str data))]
        (.getBytes ^String rendered ^String charset)))
    muuntaja-format/EncodeToOutputStream
    (encode-to-output-stream [_ data charset]
      (fn [^OutputStream output-stream]
        (let [view (get (meta data) :view-fn)
              rendered (view data)]
          (.write output-stream (.getBytes ^String rendered ^String charset)))))))

(defn muuntaja-instance
  "Create a Muuntaja instance that includes HTML handling

  Can take options just like [[muuntaja.core/create]],
  see [[muuntaja.core/default-options]]."
  ([]
   (muuntaja-instance m/default-options))
  ([opts]
   (m/create
    (-> opts
        (assoc :default-format "text/html")
        (assoc-in [:formats "text/html"]
                  (muuntaja-format/map->Format
                   {:name :html
                    :encoder [html-encoder]}))))))

(defn view-fn-interceptor
  "Interceptor for handling HTML responses with a data `:body` and `:view` fn

  If the user agent requested a HTML response, and the response body returned
  from the route is a Clojure collection, and the response contains a `:view`
  function, then attach that view to the `:body` metadata, so the Muuntaja
  encoder has everything it needs to do the rendering, see [[html-encoder]].

  By default will compose the view function with `lambdaisland.hiccup/render`,
  but you can pass an alternative `:render-fn` if you want to handle the output
  of the view functions in a different way. It should return a HTML response
  body as a string."
  ([]
   (view-fn-interceptor nil))
  ([{:keys [render-fn]
     :or {render-fn hiccup/render}}]
   {:name ::view-fn
    :leave
    (fn [ctx]
      (cond
        (not= "text/html" (some-> ctx :request :muuntaja/response .-format))
        ctx

        (and (nil? (get-in ctx [:response :body]))
             (nil? (get-in ctx [:response :view])))
        ctx

        (string? (get-in ctx [:response :body]))
        ctx

        (and (coll? (get-in ctx [:response :body]))
             (get-in ctx [:response :view]))
        (update-in ctx [:response :body]
                   vary-meta
                   assoc
                   :view-fn (comp hiccup/render (get-in ctx [:response :view])))

        (and (nil? (get-in ctx [:response :body]))
             (get-in ctx [:response :view]))
        (assoc-in ctx [:response :body]
                  ^{:view-fn (comp hiccup/render (get-in ctx [:response :view]))}
                  {})

        (get-in ctx [:response :html])
        (assoc-in ctx [:response :body]
                  ^{:view-fn (fn [_] (hiccup/render-html (get-in ctx [:response :html])))}
                  {})

        :else
        (assoc ctx
               :response
               {:status 415
                :headers {"Content-Type" "text/html"}
                :body "<h1>Unsupported Media Type. No HTML view defined for route.</h1>"})))}))

(defn inject-components-interceptor
  "Generic interceptor for inject system components into the request map

  Takes a map which will be merged into each request, so its contents are
  available to route implementations."
  [components]
  {:name ::inject-components
   :enter
   (fn [ctx]
     (update ctx :request merge components))})

(defn log-request-interceptor
  "Interceptor to log requests

  Logs request start at the `trace` level, and request end at the `info` level.
  Includes the total time it took to handle the request."
  []
  {:name ::log-request
   :enter
   (fn [ctx]
     (log/trace :request/starting (select-keys (:request ctx) [:request-method :uri]))
     (assoc ctx ::start-time (System/nanoTime)))
   :leave
   (fn [ctx]
     (let [{:keys [request response]} ctx
           time (format "%.2fms" (/ (- (System/nanoTime) (::start-time ctx)) 1e6))]
       (log/info :request/done {:method (:request-method request)
                                :uri (:uri request)
                                :status (:status response)
                                :content-type (get (:headers response) "Content-Type")
                                :time time}))
     ctx)})

(defn exception-handler
  "Render 500 errors, but log them as well."
  [^Throwable error request]
  (log/error :handler/error {:method (:request-method request)
                             :uri (:uri request)}
             :exception error)
  {:status 500
   :body
   ^{:view-fn
     (fn [{:keys [type class message]}]
       (hiccup/render
        [:div
         [:h1 "500 Server Error"]
         [:h2 class]
         [:p message]]))}
   {:type "exception"
    :class (.getName (.getClass error))
    :message (.getMessage error)}})

(defn ring-default-handler
  "The default fallback handler

  - Strip trailing slashes (will cause a redirect)
  - Handler 404/405/406 responses, see [[reitit.ring/create-default-handler]]
    for options
  "
  ([]
   (ring-default-handler nil))
  ([opts]
   (ring/routes
    (ring/redirect-trailing-slash-handler {:method :strip})
    (ring/create-default-handler opts))))

(defn default-interceptors
  "Default interceptor chain

  Includes content negotiation, HTML view handling, exception handling, and
  request logging.
  "
  []
  [(log-request-interceptor)
   (muuntaja-interceptor/format-interceptor)
   (parameters/parameters-interceptor)
   (muuntaja/format-negotiate-interceptor)
   (muuntaja/format-response-interceptor)
   (exception/exception-interceptor (assoc exception/default-handlers ::exception/default exception-handler))
   (muuntaja/format-request-interceptor)
   (coercion/coerce-response-interceptor)
   (coercion/coerce-request-interceptor)
   (multipart/multipart-interceptor)
   (view-fn-interceptor)])

(defn ring-handler
  "Build up a ring handler based on Reitit and Sieppari

  Takes a collection of reitit `:routes`, and optionally a `:muuntaja-instance`,
  a sequence of `:interceptors`, and a `:default-handler` which is used when no
  route matches. See [[muuntaja-instance]], [[default-interceptors]]
  and [[ring-default-handler]] for the default values.

  Can also optionally take a sequence of `:middleware`, which is handled through
  `reitit.middleware`, so it accepts anything that implements `IntoMiddleware`."
  [{:keys [muuntaja-instance interceptors default-handler routes middleware]
    :or {muuntaja-instance (muuntaja-instance)
         interceptors (default-interceptors)
         default-handler (ring-default-handler)}}]
  (let [wrap (if (seq middleware)
               (partial middleware/chain middleware)
               identity)
        handler (http/ring-handler
                 (http/router
                  routes
                  {:data {:muuntaja muuntaja-instance
                          :interceptors interceptors}})
                 default-handler
                 {:executor sieppari/executor})]
    (with-meta (wrap handler) (meta handler))))

(defn start-jetty!
  "Start a Jetty instance and start listening for requests

  The `:port` must be specified. Takes a `:build-handler` function which should
  return a valid Ring handler function. When `:rebuild-on-request?` is `true`
  this build-handler is called on every request. This is useful in development
  for ensuring that changes made on the REPL are picked up, but is not
  recommended for production use, since rebuilding the handler (and with it, the
  router) is expensive.

  `:wrap-handler` can be used to wrap the handler in static middleware, this
  does not get updated on each request.

  Returns the Jetty instance"
  [{:keys [port rebuild-on-request? build-handler join? wrap-handler]
    :or {join? false
         wrap-handler identity}}]
  (log/info :server/starting {:port port :rebuild-on-request? rebuild-on-request?})
  (jetty/run-jetty (wrap-handler
                    (if rebuild-on-request?
                      #((build-handler) %)
                      (build-handler)))
                   {:port port
                    :join? false}))

(defn stop-jetty!
  "Stop a Jetty instance"
  [^Server jetty]
  (log/info :server/stopping {:uri (str (.getURI jetty))})
  (.stop jetty))
