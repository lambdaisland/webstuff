(ns ^{:ornament/prefix "ot"}
    lambdaisland.ornament-test
  (:require [lambdaisland.ornament :as o]
            [clojure.test :refer [deftest testing is are use-fixtures run-tests join-fixtures]]
            #?(:clj [lambdaisland.hiccup :as hiccup]
               :cljs [lambdaisland.thicc :as thicc])))

(defn render [h]
  #?(:clj (hiccup/render h {:doctype? false})
     :cljs (.-outerHTML (thicc/dom h))))

(o/defstyled simple :span
  {:color "#ffffff"})


(o/defstyled tokens :span
  :px-5 :py-3 :rounded-xl)

(o/defstyled combined :span
  :px-5 :py-3 :rounded-xl
  {:color "azure"})

(o/defstyled nested :ul
  :px-3
  [:li {:list-style :square}])

(o/defstyled with-body :p
  :px-5 :py-3 :rounded-xl
  {:color "azure"}
  ([props & children]
   (into [:strong props] children)))

(o/defstyled time :time
  :border
  :border-black
  ([{:keys [date time]}]
   ^{:datetime (str date " " time)}
   [:<> date " " time]))

(o/defstyled ornament-in-ornament :div
  {:color "blue"}
  [simple {:color "red"}])

(o/defstyled base :span
  {:color "blue"
   :background-color "red"})

(o/defstyled inherited base
  {:color "green"
   :list-style :square})

(def my-tokens {:main-color "green"})

(o/defstyled with-code :div
  {:background-color (-> my-tokens :main-color)})

(o/defstyled with-media :div
  {:padding "0 1rem 1rem"}
  [:at-media {:min-width "1rem"}
   {:grid-gap "1rem"
    :padding "0 2rem 2rem"}])

#?(:clj
   (deftest css-test
     (is (= ".ot__simple{color:#fff}"
            (o/css simple)))

     (is (= ".ot__tokens{padding-left:1.25rem;padding-right:1.25rem;padding-top:.75rem;padding-bottom:.75rem;border-radius:.75rem}"
            (o/css tokens)))

     (is (= ".ot__combined{padding-left:1.25rem;padding-right:1.25rem;padding-top:.75rem;padding-bottom:.75rem;border-radius:.75rem;color:azure}"
            (o/css combined)))

     (is (= ".ot__nested{padding-left:.75rem;padding-right:.75rem}.ot__nested li{list-style:square}"
            (o/css nested)))

     (is (= ".ot__with_body{padding-left:1.25rem;padding-right:1.25rem;padding-top:.75rem;padding-bottom:.75rem;border-radius:.75rem;color:azure}"
            (o/css with-body)))

     (is (= ".ot__ornament_in_ornament{color:blue}.ot__ornament_in_ornament ot__simple{color:red}"
            (o/css ornament-in-ornament)))

     (is (= ".ot__inherited{color:green;background-color:red;list-style:square}"
            (o/css inherited)))

     (is (= ".ot__with_code{background-color:green}"
            (o/css with-code)))

     (is (= ".ot__with_media{padding:0 1rem 1rem}@media(min-width:1rem){.ot__with_media{grid-gap:1rem;padding:0 2rem 2rem}}"
            (o/css with-media)))))

(deftest rendering-test
  (are [hiccup html] (= html (render hiccup))
    [simple]
    "<span class=\"ot__simple\"></span>"

    [simple {:class "xxx"}]
    "<span class=\"ot__simple xxx\"></span>"

    [simple {:class "xxx"} [:strong "child"]]
    "<span class=\"ot__simple xxx\"><strong>child</strong></span>"

    [simple {:class "xxx" :style {:border-bottom "1px solid black"}} [:strong "child"]]
    "<span class=\"ot__simple xxx\" style=\"border-bottom: 1px solid black;\"><strong>child</strong></span>"

    [time {:date "2021-06-25" :time "10:11:12"}]
    "<time datetime=\"2021-06-25 10:11:12\" class=\"ot__time\">2021-06-25 10:11:12</time>"
    [simple {:class time}]
    "<span class=\"ot__simple ot__time\"></span>"

    [simple {:class [time]}]
    "<span class=\"ot__simple ot__time\"></span>"))

(o/defstyled custok1 :div
  :bg-primary)

(o/defstyled custok2 :div
  :font-system)

(o/defstyled custok3 :div
  :full-center)

(o/defstyled custok4 :div
  :full-center-bis)

(o/defstyled custok5 :ul
  :bullets-🐻)

(deftest custom-tokens-test
  (o/set-tokens! {:colors {:primary "001122"}
                  :fonts {:system "-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji"}
                  :components [{:id :full-center
                                :garden {:display "inline-flex"
                                         :align-items "center"}}
                               {:id :full-center-bis
                                :garden [:& :inline-flex :items-center]}
                               {:id :custom-bullets
                                :rules "custom-bullets = <'bullets-'> bullet-char
                                      <bullet-char> = #\".\""
                                :garden (fn [{[bullet-char] :component-data}]
                                          [:&
                                           {:list-style "none"
                                            :padding 0
                                            :margin 0}
                                           [:li
                                            {:padding-left "1rem"
                                             :text-indent "-0.7rem"}]
                                           ["li::before"
                                            {:content bullet-char}]])}]})


  (is (= ".ot__custok1{--gi-bg-opacity:1;background-color:rgba(0,17,34,var(--gi-bg-opacity))}"
         (o/css custok1)))

  (is (= ".ot__custok2{font-family:-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji}"
         (o/css custok2)))

  (is (= ".ot__custok3{display:inline-flex;align-items:center}"
         (o/css custok3)))

  (is (= ".ot__custok4{display:inline-flex;align-items:center}"
         (o/css custok4)))

  (is (= ".ot__custok5{list-style:none;padding:0;margin:0}.ot__custok5 li{padding-left:1rem;text-indent:-0.7rem}.ot__custok5 li::before{content:🐻}"
         (o/css custok5)))

  (o/set-tokens! {}))

(deftest meta-merge-tokens-test
  ;; establish baseline
  (is (= {:--gi-bg-opacity 1, :background-color "rgba(239,68,68,var(--gi-bg-opacity))"}
         (o/process-rule :bg-red-500)))

  (is (= {:font-family "ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, \"Liberation Mono\", \"Courier New\", monospace"}
         (o/process-rule :font-mono)))

  (is (= {:border-radius "0.75rem"}
         (o/process-rule :rounded-xl)))

  ;; Replace the default colors/fonts, leave the components so we can still do bg-* or font-*
  (o/set-tokens! {:colors ^:replace {:primary "001122"}
                  :fonts ^:replace {:system "-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji"}})

  ;; The built-in ones are all gone
  (is (nil? (o/process-rule :bg-red-500)))
  (is (nil? (o/process-rule :font-mono)))

  (is {:--gi-bg-opacity 1, :background-color "rgba(0,17,34,var(--gi-bg-opacity))"}
      (o/process-rule :bg-primary))

  (is {:font-family "-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji"}
      (o/process-rule :font-system))


  ;; Replace the components
  (o/set-tokens! {:components ^:replace [{:id :full-center
                                          :garden {:display "inline-flex"
                                                   :align-items "center"}}]})

  (is (= {:display "inline-flex", :align-items "center"}
         (o/process-rule :full-center)))

  (is (nil? (o/process-rule :rounded-xl)))

  ;; Reset to defaults
  (o/set-tokens! {}))

(deftest defined-styles-test
  (let [reg @o/registry]
    (reset! o/registry {})

    ;; Deal with the fact that the registry is populated at compile time
    (eval
     `(o/defstyled ~'my-styles :div
        {:color "red"}))

    (eval
     `(o/defstyled ~'more-styles :span
        :rounded-xl))

    (is (= ".user__my_styles{color:red}\n.user__more_styles{border-radius:.75rem}"
           (o/defined-styles)))

    (reset! o/registry reg)))

(o/defstyled tea :div
  :bg-red-200
  :md:text-green-900
  {:color "MediumSeaGreen"})
