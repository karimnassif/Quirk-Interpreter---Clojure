(ns clojure-quirk.core
  (:gen-class)
  (:require [instaparse.core :as insta]))

(defn third [alist] (nth alist 2))
(defn fourth [alist] (nth alist 3))

(defn ret-print [thingToPrint] 
  (println thingToPrint)
  thingToPrint
)

(defn CallByLabel [funLabel & args]
  (apply(ns-resolve 'clojure-quirk.core (symbol(name funLabel))) args))


;Program --> Statement Program | Program     
(defn Program [subtree scope]   
  (println "PROGRAM")
  ;Program0
  (cond (= 3 (count subtree))
        ((CallByLabel (first (second subtree)) (second subtree) scope)
        (CallByLabel (first (third subtree)) (third subtree) scope))
   ;Program1
   :else
        (CallByLabel (first (second subtree)) (second subtree) scope)
   )
 )

;Statement := FunctionDeclaration | Assignment | Print
(defn Statement [subtree scope]
  (println "STATEMENT")
  (cond (= :FunctionDeclaration (first (second subtree)))
              (CallByLabel (first (second subtree))(second subtree) scope)
        (= :Assignment (first (second subtree)))
              (CallByLabel (first (second subtree))(second subtree) scope)
        (= :Print (first (second subtree)))
              (CallByLabel (first (second subtree))(second subtree) scope))
  )

;Print := PRINT Expression
(defn Print [subtree scope]
  (println "PRINT")
  (ret-print (CallByLabel (first (third subtree))(third subtree) scope))
)
        

;Expression -->  Term ADD Expression | Term SUB Expression | Term    
(defn Expression [subtree scope]
  (println "EXPRESSION")
  (cond (= 2 (count subtree))
        (CallByLabel (first (second subtree))(second subtree) scope)
        
        (= :ADD (first (third subtree)))
			          (+ (CallByLabel (first (second subtree))(second subtree) scope)
			          (CallByLabel (first (fourth subtree))(fourth subtree) scope))
		    (= :SUB (first (third subtree)))
			           (- (CallByLabel (first (second subtree))(second subtree) scope)
			           (CallByLabel (first (fourth subtree))(fourth subtree) scope))
    )
 ); end Expression

;Term := Factor MULT Term | Factor DIV Term | Factor
(defn Term [subtree scope] 
  (println "TERM")
  (cond (= 2 (count subtree))
             (CallByLabel (first (second subtree))(second subtree) scope)
        (= :MULT (first (third subtree)))
             (* (CallByLabel (first (second subtree))(second subtree) scope)
             (CallByLabel (first (fourth subtree))(fourth subtree) scope))
        (= :DIV (first (third subtree)))
             (/ (CallByLabel (first (second subtree))(second subtree) scope)
             (CallByLabel (first (fourth subtree))(fourth subtree) scope)))
   )

;Factor := SubExpression EXP Factor | SubExpression | FunctionCall | Value EXP Factor | Value
;              <*     ---------------------          *> TODO:FIX, fix a lot 
(defn Factor [subtree scope] 
  (println "FACTOR")
  (cond (= 3 (count subtree))
             (println "Gotta do exp")
             ;(^ (CallByLabel (first (second subtree))(second subtree) scope)
             ;(CallByLabel (first (fourth subtree))(fourth subtree) scope))
        (= :FunctionCall (first (second subtree))
             (CallByLabel (first (second subtree))(second subtree) scope))
        (= :Value (first (second subtree))
             (CallByLabel (first (second subtree))(second subtree) scope))
        )
  )
  
;Value := Name | MyNumber
(defn Value [subtree scope]
  (println "VALUE")
  (cond (= :Name (first (second subtree)))
             (CallByLabel (first (second subtree))(second subtree) scope)
       (= :MyNumber (first (second subtree)))
             (CallByLabel (first (second subtree))(second subtree) scope))
  )

;Number := NUMBER | SUB NUMBER | ADD NUMBER
(defn MyNumber [subtree scope]
  (println "NUMBER")
  (println (second (second subtree)))
  (cond (= :SUB (first (second subtree)))
        (println "lol")
                 ;(-(Double/parseDouble (second subtree)))
        (= :ADD (first (second subtree)))     
        (println "lol")
                 ;(Double/parseDouble (third subtree))
        :else
                 (Double/parseDouble (second (second subtree)))
        )
  )
        
(defn interpret-quirk [subtree scope] 
  (println "Interpreting")
  (CallByLabel (first subtree) subtree {} ))

(defn -main [& args]
 (if (.equals "-pt" (first *command-line-args*))
   (def SHOW_PARSE_TREE true)
 )
 (def quirk-parser (insta/parser (slurp "resources/quirk-grammar-ebnf") :auto-whitespace :standard))
 ;(def parse-tree (quirk-parser "function foo(x){return x+5} print foo"))
 (def parse-tree (quirk-parser "print 5"))
 (if(= true SHOW_PARSE_TREE)
       ;(println parse-tree)
       ;(println (count parse-tree))
       (interpret-quirk parse-tree {})
)
)