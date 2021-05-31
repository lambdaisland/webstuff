(ns lambdaisland.ornament
  (:require [clojure.string :as str]
            [garden.compiler :as gc]
            [garden.core :as garden]
            [garden.color :as gcolor]
            [garden.types :as gt]
            [garden.stylesheet :as gs]
            #?(:clj [lambdaisland.hiccup :as hiccup]))
  #?(:cljs
     (:require-macros [lambdaisland.ornament :refer [defstyled]])))

(defprotocol Style
  (classname [_])
  (as-garden [_])
  (css [_])
  (rules [_]))

#?(:clj
   (defmethod print-method ::styled [x writer]
     (.write writer (classname x))))

(def munge-map
  {\@ "_CIRCA_"
   \! "_BANG_"
   \# "_SHARP_"
   \% "_PERCENT_"
   \& "_AMPERSAND_"
   \' "_SINGLEQUOTE_"
   \* "_STAR_"
   \+ "_PLUS_"
   \- "_"
   \/ "_SLASH_"
   \: "_COLON_"
   \[ "_LBRACK_"
   \{ "_LBRACE_"
   \< "_LT_"
   \\ "_BSLASH_"
   \| "_BAR_"
   \= "_EQ_"
   \] "_RBRACK_"
   \} "_RBRACE_"
   \> "_GT_"
   \^ "_CARET_"
   \~ "_TILDE_"
   \? "_QMARK_"})

(defn munge-str [s]
  #?(:clj
     (let [sb (StringBuilder.)]
       (doseq [ch s]
         (if-let [repl (get munge-map ch)]
           (.append sb repl)
           (.append sb ch)))
       (str sb))
     :cljs
     (apply str (map #(get munge-map % %) s))))

(defn classname-for [varsym]
  (let [prefix (or #_(:ornament/prefix (meta (the-ns (symbol (namespace varsym)))))
                   (-> varsym
                       namespace
                       #_  (str/replace #"lambdaisland\." "")
                       (str/replace #"\." "_")))]
    (str prefix "__" (munge-str (name varsym)))))

(defn join-vector-by [sep val]
  (if (vector? val)
    (str/join sep val)
    val))

(declare process-rule)

(defmulti process-tag (fn [[tag & _]] tag))

(defmethod process-tag :default [v]
  (mapv process-rule v))

(defmethod process-tag :at-media [[_ media-queries & rules]]
  (gs/at-media media-queries (into [:&] (map process-rule) rules)))

(defmethod process-tag :cssfn [[_ fn-name & args]]
  (gt/->CSSFunction fn-name args))

(defmethod process-tag :at-font-face [[_ & props]]
  ["@font-face" props])

(defmethod process-tag :at-import [[_ url & media-queries]]
  (gt/->CSSAtRule :import {:url url :media-queries media-queries}))

(defmethod process-tag :at-supports [[_ feature-queries & rules]]
  (gt/->CSSAtRule :feature {:feature-queries feature-queries
                            :rules (into [:&] rules)}))

(defmethod process-tag :at-keyframes [[_ identifier & frames]]
  (gt/->CSSAtRule :keyframes {:identifier identifier
                              :frames frames}))

(defmethod process-tag :rgb [[_ r g b]]
  (gcolor/rgb [r g b]))

(defmethod process-tag :hsl [[_ h s l]]
  (gcolor/hsl [h s l]))

(defmulti process-property (fn [prop val] prop))

(defmethod process-property :default [_ val]
  (if (vector? val)
    (process-tag val)
    val))

(defmethod process-property :grid-template-areas [_ val]
  (if (vector? val)
    (str/join " "
              (map (fn [row]
                     (pr-str (str/join " " (map name row))))
                   val))
    val))

(defmethod process-property :grid-area [_ val] (join-vector-by " / " val))
(defmethod process-property :border [_ val] (join-vector-by " " val))
(defmethod process-property :margin [_ val] (join-vector-by " " val))
(defmethod process-property :padding [_ val] (join-vector-by " " val))

(defn process-rule [rule]
  (cond
    (record? rule) ; Prevent some defrecords in garden.types to be fudged
    rule

    (map? rule)
    (into {} (map (fn [[k v]] [k (process-property k v)])) rule)

    (vector? rule)
    (process-tag rule)

    :else
    rule))

(defn process-rules [rules]
  (map process-rule rules))

(defn styled
  ([varsym rules]
   (styled varsym :div rules))
  ([varsym tag rules]
   #?(:clj
      (let [classname (classname-for varsym)]
        ^{:type ::styled}
        (reify
          Style
          (classname [_] classname)
          (as-garden [_] (into [(str "." classname)] (process-rules rules)))
          (css [this] (gc/compile-css
                       {:pretty-print? false}
                       (as-garden this)))
          (rules [_] rules)

          clojure.lang.IFn
          (invoke [_] classname)
          (invoke [this a] (str this " " a))
          (invoke [this a b] (str this " " a " " b))
          (invoke [this a b c] (str this " " a " " b " " c))
          (invoke [this a b c d] (str this " " a " " b " " c " " d))
          (invoke [this a b c d e] (str this " " a " " b " " c " " d " " e))
          (invoke [this a b c d e f] (str this " " a " " b " " c " " d " " e " " f))
          (invoke [this a b c d e f g] (str this " " a " " b " " c " " d " " e " " f " " g))
          (invoke [this a b c d e f g h] (str this " " a " " b " " c " " d " " e " " f " " g " " h))
          (invoke [this a b c d e f g h i] (str this " " a " " b " " c " " d " " e " " f " " g " " h " " i))
          (invoke [this a b c d e f g h i j] (str this " " a " " b " " c " " d " " e " " f " " g " " h " " i " " j))
          (invoke [this a b c d e f g h i j k] (str this " " a " " b " " c " " d " " e " " f " " g " " h " " i " " j " " k))
          (invoke [this a b c d e f g h i j k l] (str this " " a " " b " " c " " d " " e " " f " " g " " h " " i " " j " " k " " l))
          (invoke [this a b c d e f g h i j k l m] (str this " " a " " b " " c " " d " " e " " f " " g " " h " " i " " j " " k " " l " " m))
          (invoke [this a b c d e f g h i j k l m n] (str this " " a " " b " " c " " d " " e " " f " " g " " h " " i " " j " " k " " l " " m " " n))
          (invoke [this a b c d e f g h i j k l m n o] (str this " " a " " b " " c " " d " " e " " f " " g " " h " " i " " j " " k " " l " " m " " n " " o))
          (invoke [this a b c d e f g h i j k l m n o p] (str this " " a " " b " " c " " d " " e " " f " " g " " h " " i " " j " " k " " l " " m " " n " " o " " p))
          (invoke [this a b c d e f g h i j k l m n o p q] (str this " " a " " b " " c " " d " " e " " f " " g " " h " " i " " j " " k " " l " " m " " n " " o " " p " " q))
          (invoke [this a b c d e f g h i j k l m n o p q r] (str this " " a " " b " " c " " d " " e " " f " " g " " h " " i " " j " " k " " l " " m " " n " " o " " p " " q " " r))
          (invoke [this a b c d e f g h i j k l m n o p q r s] (str this " " a " " b " " c " " d " " e " " f " " g " " h " " i " " j " " k " " l " " m " " n " " o " " p " " q " " r " " s))

          Object
          (toString [_] classname)

          gc/IExpandable
          (expand [this]
            (mapcat
             (fn [rule]
               (gc/expand
                (if (map? rule)
                  [:& rule]
                  rule)))
             rules))

          hiccup/HiccupTag
          (-expand [_ attrs children]
            (into [tag (update attrs :class #(conj (if (coll? %)
                                                     %
                                                     [(str %)])
                                                   classname))] children))))
      :cljs
      (let [classname (classname-for varsym)]
        (specify!
            (fn [?props & children]
              (let [[props children] (if (map? ?props)
                                       [?props children]
                                       [nil (cons ?props children)])
                    props (update props :class #(if % (str % " " classname) classname))]
                (into [tag props] children)))
          Object
          (toString [] classname) )))))

#?(:clj
   (defmacro defstyled [sym el & styles]
     (let [varsym (symbol (name (ns-name *ns*)) (name sym))]
       `(def ~(with-meta sym {::css true})
          (styled '~varsym '~el (list ~@styles))))))

#?(:clj
   (defmacro defcss [sym & styles]
     `(defstyled ~sym :div ~@styles)))

#?(:clj
   (defn defined-styles []
     (str/join "\n"
               (sequence
                (comp (mapcat ns-publics)
                      (map val)
                      (filter (comp ::css meta))
                      (map (comp css deref)))
                (all-ns)))))

(comment

  ;; This namespace is a first iteration of an experimental mini-library to do
  ;; css-in-js style component-scoped CSS in a server-rendered Clojure application
  ;; using Garden.

  ;; The main entry point is the `defcss` macro, like other `def...` macros it
  ;; takes a symbol and one or more body forms, and defines a var.

  ;; You can use a map to define CSS properties

  (defcss tea
    {:color "MediumSeaGreen"})
  ;; => #'lambdaisland.ornament/tea

  ;; Or have vectors to define styles for nested elements, you have full garden
  ;; syntax available.

  (defcss chocolate
    {:color "chocolate"}
    [:a {:color "coral"}])
  ;; => #'lambdaisland.ornament/chocolate

  ;; These vars can do a bunch of different things, for instance you could see what
  ;; Garden styles they contain

  ;; You can look at the rules they contain
  (rules chocolate)
  ;; => ({:color "chocolate"} [:a {:color "coral"}])

  ;; See what class name is associated with this style
  (classname chocolate)
  ;; => "ornament__chocolate"
  ;; (or just (str chocolate))

  ;; See the full definition of this style in garden syntax, as it will end up
  ;; in the final stylesheet.
  (as-garden tea)
  ;; => [".ornament__tea" {:color "MediumSeaGreen"}]

  ;; Or the same thing, but already compiled to CSS

  (css tea)
  ;; => ".ornament__tea{color:MediumSeaGreen}"

  ;; In this case the definition `(defcss tea)` led to the class
  ;; name "ornament__tea", because the definition is inside the
  ;; `lambdaisland.ornament` namespace. If it was in the
  ;; `lambdaisland.collections.views` namespace it would have been
  ;; "collection_views__tea".

  ;; So we take the full var name including namespace name, something like like
  ;; lambdaisland.ornament/tea, chop off the lambdaisland bit, replace "."
  ;; with "_", and "/" with "__", and that's your final classname.

  ;; The end result is a classname which is
  ;; - Globally unique, because it's namespaced
  ;; - Human readable
  ;; - Corresponds closely to the code, so easy to search for when debugging

  ;; But in some cases this will not really be optimal, especially with long and
  ;; deeply nested namespace names. In that case you can specify a different
  ;; prefix (the part before the "__") to use, with namespace metadata. Add
  ;; an :ornament/prefix key to the namespace metadata, and this will be used
  ;; instead. Be careful though, because if you use the same prefix for two
  ;; namespaces your names may no longer be globally unique.

  ;; So for example, say you have this namespace

  (ns ^{:ornament/prefix "admin"} lambdaisland.admin.ui.components)

  ;; and in there this style

  (defcss widget)

  ;; then this would be the classname

  (classname widget)
  ;;=> "admin__widget"


  ;; You can of course also look at what the actual compiled CSS is.


  ;; To see the class name instead of using the `classname` function, you can
  ;; also just convert to a string with `str`.

  (str tea)
  ;; => "ornament__tea"
  (str chocolate)
  ;; => "ornament__chocolate"

  ;; This is convenient, because this way we can use it directly inside hiccup

  (def html
    [:div {:class tea}])

  (require '[net.cgrand.enlive-html :as enlive])
  (apply str (enlive/emit* (enlive/html html)))
  ;; => "<div class=\"ornament__tea\"></div>"

  ;; But what if you want to add more than one class? You could do something like this

  (def html
    [:div {:class (str tea " " chocolate)}])

  ;; but that's tedious. To make this nicer these styles have one more trick up
  ;; their sleeve, when used as a function they convert themselves to a string,
  ;; as well as all their arguments, joining them with spaces.

  (tea chocolate)
  ;; => "ornament__tea ornament__chocolate"

  ;; In other words, you can always just put a list of style names in your hiccup,
  ;; and it will turn into a list of classes.

  (defcss bold
    {:font-weight "bold"})
  ;; => #'lambdaisland.ornament/bold

  [:div {:class (tea chocolate bold)}]
  ;; => [:div {:class "ornament__tea ornament__chocolate ornament__bold"}]

  ;; So this is the main thing that `defcss` does, it allows you to define styles
  ;; which all get gathered up in a single stylesheet, and you get a classname back
  ;; to use in your Hiccup.

  ;; But sometimes you might have style definitions that you want to use inside
  ;; other garden definitions, say you have a utility like this:

  (defcss text-xl
    {:font "3rem/1.208 rubik, sans-serif"
     :letter-spacing "0.125rem"})

  (defcss text-l
    {:font "36px/56px rubik, sans-serif"
     :letter-spacing "1px"})

  ;; You can imagine two ways this will be used, in some default styles

  (def ^{:garden {:output-to "styles.css"}}
    styles
    [[:h1 text-xl]
     [:h2 text-l]])

  ;; As well as in specific markup

  (defn big-bullets [items]
    [:ul {:class text-l}
     [:li ...]])

  ;; To support both these use cases it is also possible to reference `defcss`
  ;; styles directly in other garden stylesheets. In this case there is no
  ;; wrapping class added, only the style rules themselves are inserted in the
  ;; given context.

  ;; You can use it in your hiccup and get the classname

  [:h1 {:class text-xl}]
  ;;=> <h1 class="my_ns__text_xl"></h1>

  ;; Or you can use it in a stylesheet as a shorthand for a group of attributes.

  (gc/compile-css
   {}
   [:.about-ep--title
    text-xl
    {:margin-bottom 0
     :color  "#21202a"}])
  ;; =>
  ;; .about-ep--title {
  ;;   margin-bottom: 0;
  ;;   color: #21202a;
  ;; }
  ;; .about-ep--title {
  ;;   font: 3rem/1.208 rubik, sans-serif;
  ;;   letter-spacing: 0.125rem;
  ;; }


  ;;;; Styled components

  (defstyled navbar :nav
    {:background-color "blue"})

  ;; Still does everything that defcss does
  (str navbar)
  ;; => "ornament__navbar"

  (garden/css [:div#navbar navbar])
  ;; => "div#navbar {\n  background-color: blue;\n}"

  ;; But you can use it directly inside Hiccup

  (require 'lambdaisland.html.transforms)

  (apply str
         (lambdaisland.html.transforms/emit-hiccup
          [:body
           [navbar {:class "my-cool-navbar"
                    :title "Hello"}
            [:a {:href "/here"} "Here"]]]))
  ;; => "<body><nav class=\"ornament__navbar my-cool-navbar\" title=\"Hello\"><a href=\"/here\">Here</a></nav></body>"
  )
