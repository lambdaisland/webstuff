(ns lambdaisland.ornament
  "CSS-in-clj(s)"
  (:require [clojure.string :as str]
            #?@(:clj [[garden.compiler :as gc]
                      [garden.core :as garden]
                      [garden.color :as gcolor]
                      [garden.types :as gt]
                      [garden.stylesheet :as gs]
                      [girouette.tw.core :as girouette]
                      [girouette.tw.typography :as girouette-typography]
                      [girouette.tw.color :as girouette-color]
                      [girouette.tw.default-api :as girouette-default]])
            [lambdaisland.hiccup.protocols :refer [HiccupTag -expand]])
  #?(:cljs
     (:require-macros [lambdaisland.ornament :refer [defstyled]])))

#?(:clj
   (do
     (def girouette-api
       (atom (girouette/make-api
              girouette-default/default-components
              {:color-map girouette-color/default-color-map
               :font-family-map girouette-typography/default-font-family-map})))

     (defn class-name->garden [n]
       ((:class-name->garden @girouette-api) n))

     (defprotocol Style
       (classname [_])
       (as-garden [_])
       (css [_])
       (rules [_])
       (tag [_]))

     (defmethod print-method ::styled [x writer]
       (.write writer (classname x)))

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
       (let [prefix (or (:ornament/prefix (meta (the-ns (symbol (namespace varsym)))))
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
       (into [(first v)] (map process-rule (next v))))

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

         (simple-keyword? rule)
         (second (class-name->garden (name rule)))

         (map? rule)
         (into {} (map (fn [[k v]] [k (process-property k v)])) rule)

         (vector? rule)
         (process-tag rule)

         :else
         rule))

     (defn process-rules [rules]
       (seq (reduce (fn [acc rule]
                      (let [r (process-rule rule)]
                        (if (and (map? r) (map? (last acc)))
                          (conj (vec (butlast acc))
                                (merge (last acc) r))
                          (conj acc r))))
                    [] rules)))))

(defn add-class [classes class]
  (cond
    (string? classes)
    [classes class]
    (seq classes)
    (conj classes class)
    :else
    [class]))

(defn expand-hiccup-tag [tag classname attrs children component]
  (if component
    (let [child (apply component (dissoc attrs :id :class :style) children)]
      (expand-hiccup-tag tag classname
                         (into (meta child)
                               (select-keys attrs [:id :class :style]))
                         [child]
                         nil))
    (into [tag (update attrs :class add-class classname)] children)))

(defn styled
  ([varsym classname tag rules component]
   #?(:clj
      ^{:type ::styled}
      (reify
        Style
        (classname [_] classname)
        (as-garden [_] (into [(str "." classname)] (process-rules rules)))
        (css [this] (gc/compile-css
                     {:pretty-print? false}
                     (as-garden this)))
        (rules [_] rules)
        (tag [_] tag)

        clojure.lang.IFn
        (invoke [this] (-expand this {} nil))
        (invoke [this a] (if (map? a)
                           (-expand this a nil)
                           (-expand this {} [a])))
        (invoke [this a b]
          (if (map? a)
            (-expand this a [b])
            (-expand this {} [a b])))
        (invoke [this a b c]
          (if (map? a)
            (-expand this a [b c])
            (-expand this {} [a b c])))
        (invoke [this a b c d]
          (if (map? a)
            (-expand this a [b c d])
            (-expand this {} [a b c d])))
        (invoke [this a b c d e]
          (if (map? a)
            (-expand this a [b c d e])
            (-expand this {} [a b c d e])))
        (invoke [this a b c d e f]
          (if (map? a)
            (-expand this a [b c d e f])
            (-expand this {} [a b c d e f])))
        (invoke [this a b c d e f g]
          (if (map? a)
            (-expand this a [b c d e f g])
            (-expand this {} [a b c d e f g])))
        (invoke [this a b c d e f g h]
          (if (map? a)
            (-expand this a [b c d e f g h])
            (-expand this {} [a b c d e f g h])))
        (invoke [this a b c d e f g h i]
          (if (map? a)
            (-expand this a [b c d e f g h i])
            (-expand this {} [a b c d e f g h i])))
        (invoke [this a b c d e f g h i j]
          (if (map? a)
            (-expand this a [b c d e f g h i j])
            (-expand this {} [a b c d e f g h i j])))
        (invoke [this a b c d e f g h i j k]
          (if (map? a)
            (-expand this a [b c d e f g h i j k])
            (-expand this {} [a b c d e f g h i j k])))
        (invoke [this a b c d e f g h i j k l]
          (if (map? a)
            (-expand this a [b c d e f g h i j k l])
            (-expand this {} [a b c d e f g h i j k l])))
        (invoke [this a b c d e f g h i j k l m]
          (if (map? a)
            (-expand this a [b c d e f g h i j k l m])
            (-expand this {} [a b c d e f g h i j k l m])))
        (invoke [this a b c d e f g h i j k l m n]
          (if (map? a)
            (-expand this a [b c d e f g h i j k l m n])
            (-expand this {} [a b c d e f g h i j k l m n])))
        (invoke [this a b c d e f g h i j k l m n o]
          (if (map? a)
            (-expand this a [b c d e f g h i j k l m n o])
            (-expand this {} [a b c d e f g h i j k l m n o])))
        (invoke [this a b c d e f g h i j k l m n o p]
          (if (map? a)
            (-expand this a [b c d e f g h i j k l m n o p])
            (-expand this {} [a b c d e f g h i j k l m n o p])))
        (invoke [this a b c d e f g h i j k l m n o p q]
          (if (map? a)
            (-expand this a [b c d e f g h i j k l m n o p q])
            (-expand this {} [a b c d e f g h i j k l m n o p q])))
        (invoke [this a b c d e f g h i j k l m n o p q r]
          (if (map? a)
            (-expand this a [b c d e f g h i j k l m n o p q r])
            (-expand this {} [a b c d e f g h i j k l m n o p q r])))
        (invoke [this a b c d e f g h i j k l m n o p q r s]
          (if (map? a)
            (-expand this a [b c d e f g h i j k l m n o p q r s])
            (-expand this {} [a b c d e f g h i j k l m n o p q r s])))

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

        HiccupTag
        (-expand [_ attrs children]
          (expand-hiccup-tag tag classname attrs children component)))
      :cljs
      (let [render-fn (fn ^{:type ::styled} [?attrs & children]
                        (expand-hiccup-tag tag
                                           classname
                                           (if (map? ?attrs) ?attrs {})
                                           (if (map? ?attrs) children (cons ?attrs children))
                                           component))
            component (specify! render-fn
                        HiccupTag
                        (-expand [_ attrs children]
                          (expand-hiccup-tag tag classname attrs children component))

                        Object
                        (toString [_] classname))]
        (js/Object.defineProperty component "name" #js {:value (str varsym)})
        component))))


#?(:clj
   (defmacro defstyled [sym tagname & styles]
     (let [varsym (symbol (name (ns-name *ns*)) (name sym))
           [styles fn-tails] (split-with (complement list?) styles)
           tagname (eval tagname)
           inherit? (satisfies? Style tagname)
           tag (if inherit?
                 (tag tagname)
                 tagname)
           styles (into (if inherit?
                          (rules tagname)
                          [])
                        styles)]
       `(def ~(with-meta sym {::css true})
          (styled '~varsym
                  ~(classname-for varsym)
                  ~tag
                  ~(into [] styles)
                  ~(when (seq fn-tails)
                     `(fn ~@fn-tails)))))))

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

  (defstyled tea :div
    {:color "MediumSeaGreen"}
    ([_]
     [:h2 "ok"])
    ([_ x]
     [:h1 "tasty" x]))
  ;; => #'lambdaisland.ornament/tea

  (tea {:class "more" :style "xxx" :stuff "etc"} "x")

  ;; Or have vectors to define styles for nested elements, you have full garden
  ;; syntax available.

  (defstyled chocolate :div
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

  (defined-styles)
  )
