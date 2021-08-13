(ns lambdaisland.thicc
  "Experimental DOM library

  Contains DOM helpers, a Hiccup implementation, and reactive atoms ([[reatom]]).

  Converts Hiccup to plain JavaScript DOM elements directly. Largely works as
  any other hiccup implementation, but uses namespaced tag and attribute names
  to deal with DOM interfaces that are XML-namespaced, e.g. [:svg/circle {:cx
  10 :cy 20 :r 5}]

  Can take atoms or atom-like things and return reactive DOM elements."
  (:require [goog.dom :as gdom]
            [clojure.string :as str]
            [camel-snake-kebab.core :as csk]
            [lambdaisland.dom-types :as dom-types]
            [lambdaisland.hiccup.protocols :refer [HiccupTag -expand]]))

(extend-type js/Node
  ITransientCollection
  (-conj! [^js this child]
    (.appendChild this child)))

(def ^:dynamic *namespaces*
  "Mapping from keyword prefixes to xml namespaces, can be used for tag or
  attribute names. e.g. :xlink/href -> http://www.w3.org/1999/xlink/href"
  {"svg" "http://www.w3.org/2000/svg"
   "xhtml" "http://www.w3.org/1999/xhtml"
   "xlink" "http://www.w3.org/1999/xlink"
   "rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
   "cc" "http://creativecommons.org/ns#"
   "dc" "http://purl.org/dc/elements/1.1/"})

;; Some shorthand for simple DOM stuff

(defn create-el [tag-ns tag]
  (if tag-ns
    (js/document.createElementNS (get *namespaces* tag-ns) tag)
    (js/document.createElement tag)))

(defn fragment [els]
  (let [fragment (js/document.createDocumentFragment)]
    (doseq [el els]
      (.appendChild ^js fragment el))
    fragment))

(defn fragment? [f]
  (= js/DocumentFragment (type f)))

(defn set-attr [el k v]
  (let [n (csk/->camelCase (name k))]
    (cond
      (qualified-keyword? k)
      (.setAttributeNS el (get *namespaces* (namespace k)) n v)

      (= "on-" (subs (name k) 0 3))
      ;; Set event handlers directly, rather than through setAttribute
      (unchecked-set el (str/lower-case n) v)

      (and (= :class k) (sequential? v))
      (.setAttribute el "class" (str/join " " v))

      (and (= :style k) (map? v))
      (doseq [[prop val] v]
        (.setProperty (.-style el) (name prop) val))

      :else
      (.setAttribute el n v))))

(defn set-attrs [el attrs]
  (doseq [[k v] attrs]
    (set-attr el k v)))

(defn parent [node]
  (.-parentNode ^js node))

(defn children [node]
  (.-children ^js node))

(defn replace-child [el old new]
  (.replaceChild ^js el new old))

(defn replace-el [old new]
  (replace-child (parent old) old new))

(defn query [selector]
  (js/document.querySelector selector))

(defn query-all [selector]
  (js/document.querySelectorAll selector))

(defn el-by-id [id]
  (js/document.getElementById id))

(defn clone-node
  ([el]
   (clone-node el true))
  ([el deep?]
   (.cloneNode el deep?)))

(defn remove-children [el]
  (replace-child (parent el) el (clone-node el false)))

;; Hiccup

(defn split-tag [tag]
  (let [tag-str (name tag)
        tag-name (re-find #"[^#\.]+" tag-str)
        id (re-find #"[#][^#\.]+" tag-str)
        kls (re-seq #"[\.][^#\.]+" tag-str)]
    [(namespace tag)
     tag-name
     (when id (subs id 1))
     (map #(subs % 1) kls)]))

(defn split-el [[tag & tail]]
  (let [[tag-ns tag id kls] (split-tag tag)]
    [tag-ns
     tag
     (cond-> (if (map? (first tail))
               (first tail)
               {})
       id
       (assoc :id id)
       (seq kls)
       (update :class str (str/join " " kls)))
     (if (map? (first tail))
       (next tail)
       tail)]))

(defn dom [hiccup]
  (cond
    (string? hiccup)
    (gdom/createTextNode hiccup)

    (vector? hiccup)
    (cond
      (= :<> (first hiccup))
      (dom (rest hiccup))

      (implements? HiccupTag (first hiccup))
      (let [[tag ?attr-map & children] hiccup]
        (dom (-expand tag
                      (if (map? ?attr-map) ?attr-map {})
                      (if (map? ?attr-map) children (cons ?attr-map children)))))

      (fn? (first hiccup))
      (dom (apply (first hiccup) (rest hiccup)))

      :else
      (let [[tag-ns tag attrs children] (split-el hiccup)
            el (create-el tag-ns tag)]
        (set-attrs el attrs)
        (when (seq children)
          (conj! el (dom children)))
        el))

    (seq? hiccup)
    (fragment (map dom hiccup))

    (implements? IAtom hiccup)
    (let [el (atom (dom @hiccup))]
      ;; FIXME: documentFragments are emptied when they are appended to the DOM,
      ;; so replaceElement based on the parent does not work.
      (add-watch hiccup ::update (fn [_ _ _ new]
                                   (let [old-el @el
                                         new-el (dom new)]
                                     ;; Might want some smarter reconcilliation at
                                     ;; some point? for now just replace
                                     ;; wholesale
                                     (reset! el new-el)
                                     (replace-el old-el new-el))))
      @el)

    :else
    (dom (str hiccup))))

;; Reactivity

(defn reatom
  "Takes a function and a number of atom-likes which act as input, the result is
  an IAtom/IDeref/IWatchable which updates (recomputes) when the source atoms
  update."
  [f & inputs]
  (let [watches (atom {})
        compute #(apply f (map deref inputs))
        val     (volatile! (compute))
        reatom  (reify
                  Object
                  (equiv [this other]
                    (-equiv this other))

                  IAtom

                  IEquiv
                  (-equiv [o other] (identical? o other))

                  IMeta
                  (-meta [_] (meta f))

                  IDeref
                  (-deref [o]
                    @val)

                  IWatchable
                  (-notify-watches [this oldval newval]
                    (doseq [[key f] @watches]
                      (f this key oldval newval)))
                  (-add-watch [this key f]
                    (swap! watches assoc key f))
                  (-remove-watch [this key]
                    (swap! watches dissoc key)))]
    (dom-types/register-printer (type reatom) 'thicc/reatom deref)
    (doseq [a inputs]
      (add-watch a reatom (fn [key this old new]
                            (let [old @val]
                              (vreset! val (compute))
                              (-notify-watches reatom old @val)))))
    reatom))
