(ns yui-compressor
  (:require [clojure.java.io :as io])
  (:import (com.yahoo.platform.yui.compressor CssCompressor)))


(with-open [w (io/writer (io/file "/tmp/ornament_yui.css"))]
  (doto (CssCompressor. (io/reader (io/file "/tmp/ornament.css")))
    (.compress w 0)))
