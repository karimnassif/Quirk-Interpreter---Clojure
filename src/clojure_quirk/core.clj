(ns clojure-quirk.core
  (:gen-class)
  (:require [instaparse.core :as insta]))

(defn third [alist] (nth alist 2))
(defn fourth [alist] (nth alist 3))
(defn fifth [alist] (nth alist 4))
(defn seventh [alist] (nth alist 6))

;Used to print while also returning the value of what is being printed.
(defn ret-print [thingToPrint] 
  (println thingToPrint)
  thingToPrint
)

;Exponential function.
(defn exp [x n]
  (reduce * (repeat n x)))

;Takes a function name and its params as input, calls the function.
(defn CallByLabel [funLabel & args]
  (apply(ns-resolve 'clojure-quirk.core (symbol(name funLabel))) args))



;Program --> Statement Program | Statement     
(defn Program [subtree scope]   
  (println "PROGRAM")
  ;Program0
  (cond (= 3 (count subtree))
        ((do(def newScope(CallByLabel (first (second subtree)) (second subtree) scope)))
        (CallByLabel (first (third subtree)) (third subtree) newScope))
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

;FunctionDeclaration := FUNCTION Name LPAREN FunctionParams LBRACE FunctionBody RBRACE
(defn FunctionDeclaration [subtree scope]
  (println "FunctionDeclaration")
  (def params(CallByLabel (first (fifth subtree))(fifth subtree) scope))
  (def body (seventh subtree))
  (def combined [params body])
  (def newScope(assoc scope(CallByLabel (first (third subtree))(third subtree) scope) combined))
  newScope
)

;FunctionParams := NameList RPAREN | RPAREN
(defn FunctionParams [subtree scope]
  (println "Functionparams")
  (cond (= 3 (count subtree))
        ((do(def return(CallByLabel(first (second subtree))(second subtree) scope)))
             return))
  :else
        []
        )

;FunctionBody := Program Return | Return
(defn FunctionBody [subtree scope]
  (cond (= 3 (count subtree))
        ((do(CallByLabel (first (second subtree)) (second subtree) scope))
        (CallByLabel (first (third subtree)) (third subtree) newScope))
   ;Program1
   :else
        (CallByLabel (first (second subtree)) (second subtree) scope)
   )
  )
        

;Return := RETURN ParameterList
(defn Return [subtree scope]
  (println "RETURN")
  (def ret (CallByLabel(first(third subtree))(third subtree) scope))
  ret
  )


;Assignment := SingleAssignment | MultipleAssignment
(defn Assignment [subtree scope]
  (println "Assignment")
  (cond 
  (= :SingleAssignment (first (second subtree)))
        (do(def newScope (CallByLabel (first (second subtree))(second subtree) scope))
          newScope)
  (= :MultipleAssignment (first (second subtree)))
        (CallByLabel (first (second subtree))(second subtree) scope)
        )
   )
  
  
  
;SingleAssignment := VAR Name ASSIGN Expression
(defn SingleAssignment [subtree scope]
  (println "SingleAssignment")
  (def newScope (assoc scope (CallByLabel (first (third subtree))(third subtree) scope)
					(CallByLabel (first (fifth subtree))(fifth subtree) scope)))
  newScope
  )
  

;Print := PRINT Expression
(defn Print [subtree scope]
  (println "PRINT")
  (print subtree)
  (ret-print (CallByLabel (first (third subtree))(third subtree) scope))
)
        

;NameList := Name COMMA NameList | Name
(defn NameList [subtree scope]
  (println "NAMELIST")
  (cond 
  (= 4 (count subtree))
     (do(def myVec [(CallByLabel (first (second subtree))(second subtree) scope) 
                    (CallByLabel (third (second subtree))(second subtree) scope)])
       (println myVec))
  :else
     (CallByLabel (first (second subtree))(second subtree) scope)
     )
  )

;ParameterList := Parameter COMMA ParameterList | Parameter
(defn ParameterList [subtree scope]
  (println "ParameterList")
  (cond (> (count subtree) 2)
        (do(def myVec [(CallByLabel (first (second subtree))(second subtree) scope) 
                    (CallByLabel (third (second subtree))(second subtree) scope)])
        (println myVec))
  :else
        (CallByLabel (first (second subtree))(second subtree) scope)
        )
  )

;Parameter := Expression | Name
(defn Parameter [subtree scope]
  (println "Parameter")
  (CallByLabel(first(second subtree))(second subtree) scope)
  )

;Expression -->  Term ADD Expression | Term SUB Expression | Term    
(defn Expression [subtree scope]
  ;(println "EXPRESSION")
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
(defn Factor [subtree scope] 
  (println "FACTOR")
    (cond 
      (and (= 4 (count subtree))(= :SubExpression (first (second subtree))))
             (exp (CallByLabel (first (second subtree))(second subtree) scope)
             (CallByLabel (first (fourth subtree))(fourth subtree) scope))
      (= :SubExpression (first (second subtree)))
             (CallByLabel (first (second subtree))(second subtree) scope)
      (= :FunctionCall (first (second subtree)))
            (CallByLabel (first (second subtree))(second subtree) scope)       
      (and (= 4 (count subtree))(= :EXP (first (third subtree))))
             (exp (CallByLabel (first (second subtree))(second subtree) scope)
             (CallByLabel (first (fourth subtree))(fourth subtree) scope))
      (= :Value (first (second subtree)))
             (CallByLabel (first (second subtree))(second subtree) scope)
    
              )
        
        )
 

;FunctionCall := Name LPAREN FunctionCallParams COLON MyNumber | Name LPAREN FunctionCallParams
(defn FunctionCall [subtree scope]
  (println "FunctionCall") 
  (def info (CallByLabel(first (second subtree))(second subtree) scope))
  (def func_name (second (second (second subtree))))
  ;(println "1")
  (def newScope (assoc scope :__parent__ scope))
  ;(println "2")
  (def paramlist (CallByLabel(first (fourth subtree))(fourth subtree) scope))
  ;(println "3")
  (def ret(CallByLabel(first (second info))(second info) newScope))
  ret
  )
  
;FunctionCallParams := ParameterList RPAREN | RPAREN
(defn FunctionCallParams [subtree scope]
  (println "FunctionCallParams")
  (cond (= 3 (count subtree))
        (CallByLabel (first (second subtree))(second subtree) scope))
  )


;SubExpression := LPAREN Expression RPAREN
(defn SubExpression [subtree scope]
  (println "SUBEXPRESSION")
  (CallByLabel (first (third subtree))(third subtree) scope)
  )
                                   
                                   
  
;Value := Name | MyNumber
(defn Value [subtree scope]
  (println "VALUE")
  (cond (= :Name (first (second subtree)))
             (CallByLabel (first (second subtree))(second subtree) scope)
       (= :MyNumber (first (second subtree)))
             (CallByLabel (first (second subtree))(second subtree) scope))
  )



;Name := IDENT | SUB IDENT | ADD IDENT
(defn Name [subtree scope]
  (println "NAME")
  (cond 
       (contains? scope (second (second subtree)))
                 (get scope (second (second subtree)))
       :else 
                 (second (second subtree))
                )
  )

;Number := NUMBER | SUB NUMBER | ADD NUMBER
(defn MyNumber [subtree scope]
  (println "NUMBER")
  (cond (= :SUB (first (second subtree)))
                 (-(Double/parseDouble (second (third subtree))))
        (= :ADD (first (second subtree)))     
                 (Double/parseDouble (second(third subtree)))
        :else
                 (Double/parseDouble (second (second subtree)))
        )
  )
        
(defn interpret-quirk [subtree scope] 
  ;(println "Interpreting")
  (CallByLabel (first subtree) subtree {} ))

(defn -main [& args]
 (if (.equals "-pt" (first *command-line-args*))
   (def SHOW_PARSE_TREE true)
 )
 (def quirk-parser (insta/parser (slurp "resources/quirk-grammar-ebnf") :auto-whitespace :standard))
 ;(def parse-tree (quirk-parser "function foo(x){return x+5} print foo"))
 (def parse-tree (quirk-parser "var x =5 function foo_func() { return 2 ^ x} print foo_func()"))
 (if(= true SHOW_PARSE_TREE)
       ;(println parse-tree)
       (interpret-quirk parse-tree {})
       
)
)