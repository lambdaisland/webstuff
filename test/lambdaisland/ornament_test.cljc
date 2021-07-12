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

(o/css ornament-in-ornament)

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
            (o/css ornament-in-ornament)))))

(deftest rendering-test
  (are [hiccup html] (= html (render hiccup))
    [simple]
    "<span class=\"ot__simple\"></span>"

    [simple {:class "xxx"}]
    "<span class=\"xxx ot__simple\"></span>"

    [simple {:class "xxx"} [:strong "child"]]
    "<span class=\"xxx ot__simple\"><strong>child</strong></span>"

    [simple {:class "xxx" :style {:border-bottom "1px solid black"}} [:strong "child"]]
    "<span class=\"xxx ot__simple\" style=\"border-bottom: 1px solid black;\"><strong>child</strong></span>"

    [time {:date "2021-06-25" :time "10:11:12"}]
    "<time datetime=\"2021-06-25 10:11:12\" class=\"ot__time\">2021-06-25 10:11:12</time>"))
