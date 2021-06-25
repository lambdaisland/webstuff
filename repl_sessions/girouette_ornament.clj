(ns girouette-ornament
  (:require [lambdaisland.ornament :as ornament :refer [defstyled]]
            [girouette.tw.core :refer [make-api]]
            [girouette.tw.typography :as typography]
            [girouette.tw.color :as color]
            [girouette.tw.default-api :as da]))

(def colors
  (assoc color/default-color-map
         "li-blue" "4d7ad1"))

(def typography
  {"sans" "ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, \"Helvetica Neue\", Arial, \"Noto Sans\", sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\", \"Noto Color Emoji\"",
   "serif" "ui-serif, Georgia, Cambria, \"Times New Roman\", Times, serif",
   "mono" "ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, \"Liberation Mono\", \"Courier New\", monospace"})

(let [{:keys [parser class-name->garden]}
      (make-api
       da/default-components
       {:color-map colors
        :font-family-map typography})]
  (def parser parser)
  (def class-name->garden class-name->garden))

(class-name->garden "bg-li-blue-50")
;; => [".bg-li-blue-50" {:background-color "#4d7ad17f"}]

;;; Make ornament Girouette-aware
(defonce monkey-patch
  (alter-var-root
   #'ornament/process-rule
   (fn [process-rule]
     (fn [rule]
       (if (simple-keyword? rule)
         (second (class-name->garden (name rule)))
         (process-rule rule))))))

(defmethod ornament/process-tag :default [v]
  (into [(first v)] (map ornament/process-rule (next v))))
;;; end monkey patching


;; Result: mix garden styles with keyword "tokens"
(defstyled my-component :section
  :bg-li-blue-50
  :m-1
  :rounded-full
  {:cursor "crosshair"}
  [:li {:foo "bar"}])

(ornament/css my-component)
;; => ".girouette-ornament__my_component{background-color:#4d7ad17f;margin:.25rem;border-radius:9999px;cursor:crosshair}.girouette-ornament__my_component li{foo:bar}"
