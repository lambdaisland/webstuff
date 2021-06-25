(ns lambdaisland.hiccup.protocols)

(defprotocol HiccupTag
  (-expand [_ attr-map children]
    "Expand this tag, gets attr map and seq of children

     Should return valid Hiccup."))
