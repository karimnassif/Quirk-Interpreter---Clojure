(ns clojure-quirk.core
  (:gen-class)
  (:require [instaparse.core :as insta]))

(defn third [alist] (nth alist 2))
(defn fourth [alist] (nth alist 3))
(defn fifth [alist] (nth alist 4))
(defn sixth [alist] (nth alist 5))
(defn seventh [alist] (nth alist 6))

;Parameterlist uses recursion that causes nested lists, this flattens them. 
(defn my-flatten [x]
  (if (coll? x)
    (mapcat my-flatten x)
    [x]))


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
  (cond (= :FunctionDeclaration (first (second subtree)))
              (CallByLabel (first (second subtree))(second subtree) scope)
        (= :Assignment (first (second subtree)))
              (CallByLabel (first (second subtree))(second subtree) scope)
        (= :Print (first (second subtree)))
              (CallByLabel (first (second subtree))(second subtree) scope))
  )

;FunctionDeclaration := FUNCTION Name LPAREN FunctionParams LBRACE FunctionBody RBRACE
(defn FunctionDeclaration [subtree scope]
  (def params(CallByLabel (first (fifth subtree))(fifth subtree) scope))
  (def body (seventh subtree))
  (def combined [params body])
  (def newScope(assoc scope(CallByLabel (first (third subtree))(third subtree) scope) combined))
  newScope
)

;FunctionParams := NameList RPAREN | RPAREN
(defn FunctionParams [subtree scope]
  (cond (= 3 (count subtree))
        (do(def myVec
             (list (CallByLabel(first (second subtree))(second subtree) scope)))
                       myVec)
                                      
  :else
        []
        )
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
  (def ret (CallByLabel(first(third subtree))(third subtree) scope))
 
  ret
  )


;Assignment := SingleAssignment | MultipleAssignment
(defn Assignment [subtree scope]
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
  (def newScope (assoc scope (CallByLabel (first (third subtree))(third subtree) scope)
					(CallByLabel (first (fifth subtree))(fifth subtree) scope)))
  newScope
  )
  

;MultipleAssignment := VAR NameList ASSIGN FunctionCall
(defn MultipleAssignment [subtree scope]
  (def names (CallByLabel (first (third subtree))(third subtree) scope))
  (def values (CallByLabel (first (fifth subtree))(fifth subtree) scope))
  (def ret (zipmap names values))
  ret 
  )
  

;Print := PRINT Expression
(defn Print [subtree scope]
  (ret-print (CallByLabel (first (third subtree))(third subtree) scope))
)
        

;NameList := Name COMMA NameList | Name
(defn NameList [subtree scope]
  (cond 
  (= 4 (count subtree))
     (do(def myVec(list (CallByLabel (first (second subtree))(second subtree) scope)
                        (CallByLabel (first (fourth subtree))(fourth subtree) scope) 
                        ))
     myVec)
  :else
     (CallByLabel (first (second subtree))(second subtree) scope)
     )
  )

;ParameterList := Parameter COMMA ParameterList | Parameter
(defn ParameterList [subtree scope]
  (cond (> (count subtree) 2)
        (do(def myVec (list (CallByLabel (first (second subtree))(second subtree) scope)
                         (CallByLabel (first (fourth subtree))(fourth subtree) scope)
                         ))
          (def flatVec (my-flatten myVec))
          flatVec)       
  :else
        (CallByLabel (first (second subtree))(second subtree) scope)
        )
  )

;Parameter := Expression | Name
(defn Parameter [subtree scope]
  (CallByLabel(first(second subtree))(second subtree) scope)
  )

;Expression -->  Term ADD Expression | Term SUB Expression | Term    
(defn Expression [subtree scope]
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
  (cond 
    (= 6 (count subtree))
      (do
		  ;Get all stored function info.
		  (def info (CallByLabel(first (second subtree))(second subtree) scope))
		  (def func_name (second (second (second subtree))))
		  ;Add global scope as local scope parent.
		  (def newScope (assoc scope :__parent__ scope))
		  (def paramlist (CallByLabel(first (fourth subtree))(fourth subtree) scope))
		  (def paramnames (first (first info)))
		  ;Create new scope with parameter values included.
      ;Checks if there is one or many params, otherwise type issues arise
      ;(trying to pass a double instead of a list)
      (cond 
            (nil? (second paramnames))
                (def finalScope (assoc newScope paramnames paramlist))
            :else
                (do
		            (def newMap (zipmap paramnames paramlist))
		            (def finalScope (merge newScope newMap))))     
      ;Get return index
      (def index (CallByLabel (first (sixth subtree))(sixth subtree) scope ))
		  ;Run and return body. 
		  (def ret(CallByLabel(first (second info))(second info) finalScope))
      (nth ret index)
    )
    :else
    (do
      ;Get all stored function info.
		  (def info (CallByLabel(first (second subtree))(second subtree) scope))
		  (def func_name (second (second (second subtree))))
		  ;Add global scope as local scope parent.
		  (def newScope (assoc scope :__parent__ scope))
		  (def paramlist (CallByLabel(first (fourth subtree))(fourth subtree) scope))
		  (def paramnames (first (first info)))
      (cond 
              (nil? (second paramnames))
                  (def finalScope (assoc newScope paramnames paramlist))
              :else
                  (do
		              (def newMap (zipmap paramnames paramlist))
		              (def finalScope (merge newScope newMap)))) 
		  ;Create new scope with parameter values included.
		  (def newMap (zipmap paramnames paramlist))
		  (def finalScope (merge newScope newMap))
		  ;Run and return body. 
		  (def ret(CallByLabel(first (second info))(second info) finalScope))
		  ret
    )		  
    )
  )
  
;FunctionCallParams := ParameterList RPAREN | RPAREN
(defn FunctionCallParams [subtree scope]
  (cond (= 3 (count subtree))
        (CallByLabel (first (second subtree))(second subtree) scope)
          )
  )


;SubExpression := LPAREN Expression RPAREN
(defn SubExpression [subtree scope]
  (CallByLabel (first (third subtree))(third subtree) scope)
  )
                                   
                                   
  
;Value := Name | MyNumber
(defn Value [subtree scope]
  (cond (= :Name (first (second subtree)))
             (CallByLabel (first (second subtree))(second subtree) scope)
       (= :MyNumber (first (second subtree)))
             (CallByLabel (first (second subtree))(second subtree) scope))
  )



;Name := IDENT | SUB IDENT | ADD IDENT
(defn Name [subtree scope]
  (cond 
       (contains? scope (second (second subtree)))
                 (get scope (second (second subtree)))
       :else 
                 (second (second subtree))
                )
  )

;Number := NUMBER | SUB NUMBER | ADD NUMBER
(defn MyNumber [subtree scope]
  (cond (= :SUB (first (second subtree)))
                 (-(Double/parseDouble (second (third subtree))))
        (= :ADD (first (second subtree)))     
                 (Double/parseDouble (second(third subtree)))
        :else
                 (Double/parseDouble (second (second subtree)))
        )
  )

;Starts the interpret chain from the head of the parse-tree.
(defn interpret-quirk [subtree scope] 
  (CallByLabel (first subtree) subtree {} ))

(defn -main [& args]
 (if (.equals "-pt" (first *command-line-args*))
   (def SHOW_PARSE_TREE true)
 )
 (def quirk-parser (insta/parser (slurp "resources/quirk-grammar-ebnf") :auto-whitespace :standard))
 (def input (slurp "ExampleN.q"))
 (def parse-tree (quirk-parser input))
 (if(= true SHOW_PARSE_TREE)
       (println parse-tree)
       )
 (interpret-quirk parse-tree {})      
)