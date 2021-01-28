(ns lambdaisland.hiccup
  "Convert Hiccup to clojure.xml style :tag/:attrs/:content maps, with support
  for :<>, function components, and extensible via protocol.

  Anywhere where we need to render Hiccup we should prefer to go through this
  namespace."
  (:require [net.cgrand.enlive-html :as enlive]
            [garden.compiler :as gc]
            [clojure.string :as str]))

(defprotocol HiccupTag
  (-expand [_ attr-map children]
    "Expand this tag, gets attr map and seq of children, should return valid Hiccup"))

(defn- attr-map? [node-spec]
  (and (map? node-spec) (not (keyword? (:tag node-spec)))))

(defn- nodify [node-spec]
  (cond
    (string? node-spec) node-spec
    (vector? node-spec)
    (let [[tag & [m & ms :as more]] node-spec]
      (cond
        (= :<> tag)
        (enlive/flatmap nodify more)

        (keyword? tag)
        (let [[tag-name & segments] (.split (name tag) "(?=[#.])")
              id (some (fn [^String seg]
                         (when (= \# (.charAt seg 0)) (subs seg 1))) segments)
              classes (keep (fn [^String seg]
                              (when (= \. (.charAt seg 0)) (subs seg 1)))
                            segments)
              node {:tag (keyword tag-name) :attrs (if (attr-map? m) m {})
                    :content (enlive/flatmap nodify (if (attr-map? m) ms more))}
              node (if id (assoc-in node [:attrs :id] id) node)
              node (if (seq classes)
                     (update-in node
                                [:attrs :class]
                                (fn [kls]
                                  (concat classes (if (string? kls) [kls] kls))))
                     node)]
          (cond-> node
            (map? (get-in node [:attrs :style]))
            (update-in [:attrs :style] (fn [style]
                                         (-> (gc/compile-css [:& style])
                                             (str/replace #"^\s*\{|\}\s*$" "")
                                             str/trim)))
            (sequential? (get-in node [:attrs :class]))
            (update-in [:attrs :class] #(str/join " " %))))

        (fn? tag)
        (nodify (apply tag more))

        (satisfies? HiccupTag tag)
        (nodify
         (-expand tag
                  (if (attr-map? m) m {})
                  (if (attr-map? m) ms more)))

        :else
        (throw (ex-info "Not a valid hiccup tag" {:tag tag :form node-spec}))))
    (sequential? node-spec) (enlive/flatmap nodify node-spec)
    (map? node-spec) (update-in node-spec [:content] (comp nodify seq))
    :else (str node-spec)))

(defn html
  "Like net.cgrand.enlive//html, but additionally support function components,
  fragments with :<>, and extensible via the HiccupTag protocol."
  [& nodes-specs]
  (enlive/flatmap nodify nodes-specs))

;;https://www.w3.org/TR/html5/syntax.html#void-elements
(def html5-void-elements
  #{:area :base :br :col :embed :hr :img :input :keygen :link :meta :param
    :source :track :wbr})

(defn render-html [h]
  (with-redefs [enlive/self-closing-tags html5-void-elements]
    (apply str "<!DOCTYPE html>\n" (enlive/emit* h))))

(defn render [h]
  (render-html (html h)))
