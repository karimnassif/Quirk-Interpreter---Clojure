(ns clojure-quirk.core
  (:gen-class)
  (:require [instaparse.core :as insta]))

;you can run this on the command line with: lein run -pt
(defn -main [& args]
 ;(println (first *command-line-args*))
 (if (.equals "-pt" (first *command-line-args*))
   (def SHOW_PARSE_TREE true)
 )
 (def quirk-parser (insta/parser (slurp "resources/quirk-grammar-ebnf") :auto-whitespace :standard))
 (def parse-tree (quirk-parser "function foo (x, y) { return 3+4 +2 / x ^ y} print foo(12, 12)"))
 (if(= true SHOW_PARSE_TREE)
       (println parse-tree)
)
)

(-main)