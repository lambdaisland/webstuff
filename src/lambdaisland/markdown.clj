(ns lambdaisland.markdown
  (:require [clojure.string :as str]
            [clojure.walk :as walk])
  (:import (com.vladsch.flexmark.ast AutoLink
                                     BlockQuote
                                     BulletList
                                     Code
                                     CodeBlock
                                     Emphasis
                                     FencedCodeBlock
                                     HardLineBreak
                                     Heading
                                     HtmlBlock
                                     HtmlCommentBlock
                                     HtmlInnerBlock
                                     HtmlInnerBlockComment
                                     HtmlEntity
                                     HtmlInline
                                     HtmlInlineComment
                                     Image
                                     ImageRef
                                     IndentedCodeBlock
                                     Link
                                     LinkRef
                                     BulletListItem
                                     OrderedListItem
                                     MailLink
                                     OrderedList
                                     Paragraph
                                     Reference
                                     SoftLineBreak
                                     StrongEmphasis
                                     Text
                                     TextBase
                                     ThematicBreak)
           (com.vladsch.flexmark.util.ast Block
                                          Node
                                          Document)
           (com.vladsch.flexmark.util.sequence BasedSequence)
           (com.vladsch.flexmark.parser Parser)
           (com.vladsch.flexmark.ext.attributes AttributesExtension
                                                AttributeNode
                                                AttributesNode)))

(defn parse-ast
  [^String s]
  (let [^Parser parser (.. (Parser/builder)
                           (extensions [(AttributesExtension/create)])
                           build)]
    (.parse parser s)))

(.get (.get AttributesExtension/NODE_ATTRIBUTES nil) nnn)

(defprotocol Convert
  (data [node])
  #_(html [node]))

(data (parse-ast "{foo=blue} **hello** {.red.blue}


* foo
* bar
{hello=world}"))

(bean (first (.getChildren nnn)))

(extend-protocol Convert
  Node
  (data [node]
    (into [(.getClass node)] (map data) (.getChildren node)))
  Document
  (data [node]
    (into [(.getClass node)] (map data) (.getChildren node)))
  #_  (html [node]
        (map html (.getChildren node)))
  ;; Paragraph
  ;; (html [node]
  ;;   {:tag "p"
  ;;    :content (map html (.getChildren node))})
  ;; StrongEmphasis
  ;; (html [node]
  ;;   {:tag "strong"
  ;;    :content (map html (.getChildren node))})
  BasedSequence
  (data [s]
    (.unescape s))
  Text
  (data [node]
    (data (.getChars node)))
  ;; Heading
  ;; (html [node]
  ;;   {:tag (str "h" (.getLevel node))
  ;;    })
  AttributesNode
  (data [node]
    (into {} (map data) (.getChildren node))
    )
  AttributeNode
  (data [node]
    [(keyword (data (.getName node))) (data (.getValue node))])
  ;; (html [node]
  ;;   (if (.isImplicitName node)
  ;;     (let [name (.unescape (.getName node))] )))
  )



AutoLink
BlockQuote
BulletList
Code
CodeBlock
Emphasis
FencedCodeBlock
HardLineBreak
Heading
HtmlBlock
HtmlCommentBlock
HtmlInnerBlock
HtmlInnerBlockComment
HtmlEntity
HtmlInline
HtmlInlineComment
Image
ImageRef
IndentedCodeBlock
Link
LinkRef
BulletListItem
OrderedListItem
MailLink
OrderedList
Paragraph
Reference
SoftLineBreak
StrongEmphasis
Text
TextBase
ThematicBreak
