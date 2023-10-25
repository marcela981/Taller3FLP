#lang eopl

;;Taller #3

;; Marcela Mazo Castro 1843612
;; Hassen David Ortiz 2177273
;; Kevin Tobar 1941369
;; https://github.com/marcela981/Taller3FLP/


;; Especificación Léxica

(define scanner-especificacion-lexica
'(
  (espacio-blanco (whitespace) skip)
  (comentario ("//" (arbno (not #\newline whitespace))) skip)
  (identificador ("@" letter (arbno (or letter digit "?"))) symbol)
  (numero (digit (arbno digit)) number)
  (numero ("-" digit (arbno digit)) number)
  (numero (digit (arbno digit) "." digit (arbno digit)) number)
  (numero ("-" digit (arbno digit) "." digit (arbno digit)) number)
  (texto (letter (arbno (or letter digit  "_" "'" "#" "?" "=" "$" "&" "." "," ";" "*" "!" "¡" "¿" "-" ":"))) string)
  (texto ((arbno letter)) string)
 )
)

;; Gramatica

(define interpretador-gramatica
  '(
    ;; Programa
    (programa (expresion) un-programa)

    ;; Body
    (cuerpo (expresion (arbno expresion)) cuerpoc)

    ;; Expresiones
    (expresion (numero) numero-lit)
    (expresion ("\""texto"\"") texto-lit)
    (expresion (identificador) var-exp)
    (expresion ("("expresion primitiva-binaria expresion")") primapp-bin-exp)
    (expresion (primitiva-unaria "("expresion")") primapp-un-exp)
    (expresion ("Si" expresion "entonces" expresion "sino" expresion "finSI") if-exp)
    (expresion ("declarar" "("  (separated-list identificador "=" expresion ";")   ")" "{" expresion"}") variableLocal-exp)
    (expresion ("procedimiento" "("(separated-list identificador",")")" "haga" expresion "finProc") procedimiento-exp)
    (expresion ("evaluar" expresion "("(separated-list expresion ",")")" "finEval") app-exp)
    (expresion ("declaraRec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion)  "{" expresion "}") 
                letrec-exp)

    
    ;; Primitiva binaria
    
    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("concat") primitiva-concat)

    ;; Primitiva unaria
    
    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1))
  )

;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

;Parser, Scanner, Interfaz

;Tipos de datos para la sintaxis abstracta ((Análisis léxico y sintáctico)
(define scan&parse
  (sllgen:make-string-parser scanner-especificacion-lexica interpretador-gramatica))

;El Analizador Léxico (Scanner)
(define just-scan
  (sllgen:make-string-scanner scanner-especificacion-lexica interpretador-gramatica))
  
(sllgen:make-define-datatypes scanner-especificacion-lexica interpretador-gramatica)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-especificacion-lexica interpretador-gramatica)))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )
(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-especificacion-lexica
      interpretador-gramatica)))

;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

;El interprete

;Función que evalua el programa


;Evalua el ambiente inicial
(define eval-program
  (lambda (pgm)
    (cases programa pgm
      (un-programa (body)
                 (evaluar-expresion body (init-env))))))

;init-env
;Lista de valores y ambiente vacío como argumentos, se usa para crear un ambiente.
;Ambiente inicial
;(define init-env
;  (lambda ()
;    (extend-env
;     '(x y z)
;     '(1 2 3)
;     (empty-env))))


(define init-env
  (lambda ()
    (extended-env
     '(@a @b @c @d @e)
     '(1 2 3 "hola" "FLP")
     (empty-env))))

;Evalua la expresión en el ambiente de entrada
(define evaluar-expresion
  (lambda (exp env)
    (cases expresion exp
      (numero-lit (datum) datum)
      (texto-lit (txt) txt)
      (var-exp (id) (buscar-variable env id))
      (primapp-un-exp (prim rand)
                   (apply-primitive-un prim (evaluar-expresion rand env)))
      (primapp-bin-exp (rand1 prim rand2)
                   (apply-primitive-bin prim (evaluar-expresion rand1 env) (evaluar-expresion rand2 env))
                   )
      (if-exp (test-exp true-exp false-exp)
              (if (valor-verdad? (evaluar-expresion test-exp env))
                  (evaluar-expresion true-exp env)
                  (evaluar-expresion false-exp env)))                   
      (variableLocal-exp (ids rands body)
               (let ((args (eval-rands rands env)))
                 (evaluar-expresion body
                                  (extended-env ids args env))))
      (procedimiento-exp (ids body)
                (cerradura ids body env))
      (app-exp (rator rands)
               (let ((proc (evaluar-expresion rator env))
                     (args (eval-rands rands env)))
                 (if (procVal? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc))))
      (letrec-exp (proc-names idss bodies letrec-body)
                  (evaluar-expresion letrec-body
                                   (extend-env-recursively proc-names idss bodies env)))
      )))     


;apply-primitive-bin: <primitiva> <expresion> <expresion> -> numero | string
(define apply-primitive-bin
  (lambda (prim args1 args2)
    (cases primitiva-binaria prim
      (primitiva-suma () (+ args1 args2))
      (primitiva-resta () (- args1 args2))
      (primitiva-multi () (* args1 args2))
      (primitiva-div () (/ args1 args2))
      (primitiva-concat () (string-append args1 args2))
      )))

;apply-primitive-un: <primitiva> <expresion> <expresion> -> numero
(define apply-primitive-un
  (lambda (prim args)
    (cases primitiva-unaria prim
      (primitiva-longitud () (string-length args))
      (primitiva-add1 () (+ args 1))
      (primitiva-sub1 () (- args 1))
      )))


;/////////////////////////////////////////////////////////Funciones Auxiliares/////////////////////////////////////////////////////////


;Función que retorna la posicion en la que el caracter esta ubicado
(define indice-lista
  (lambda (pred lst)
    (indice-lista-aux 0 pred lst)))

; Función auxiliar que realiza el llamado recursivo, usando un contador auxiliar que ayuda a determinar el recorrido de la lista
(define indice-lista-aux
  (lambda (n pred lst)
    (if (null? lst)
        'NoCumple
        (if (pred (car lst))
            n
            (indice-lista-aux (+ n 1) pred (cdr lst))))))

;buscar-pos
;Funcion auxiliar que retorna la posición del elemento
(define (buscar-pos lst n)
  (if (zero? n)
      (car lst)
      (buscar-pos (cdr lst) (- n 1))))


;/////////////////////////////////////////////////////////Ambientes/////////////////////////////////////////////////////////


;****Gramatica*******
;<env-exp> ::= (empty-env)
;          ::= (extend-env <list-of symbol>
;                          <list-of scheme-value> <env-exp>)

;Representación
(define-datatype environment environment?
   (empty-env)
   (extended-env (syms (list-of symbol?))
                        (vals (list-of scheme-value?))
                        (env environment?))
   (recursively-extended-env-record
                        (proc-names (list-of symbol?))
                        (idss (list-of (list-of symbol?)))
                        (bodies (list-of expresion?))
                        (env environment?))
  )

(define scheme-value? (lambda (v) #t))


;Función que crea un ambiente (Recursivo))
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (recursively-extended-env-record
     proc-names idss bodies old-env)))

;función que busca un símbolo
(define buscar-variable
  (lambda (env sym)
    (cases environment env
      ;No se encontró la variable buscada
      (empty-env ()
                   (eopl:error '"Error, la variable no existe"))
      ;En la lista de variables busca si esta la que se requiere
      (extended-env (syms vals env)
                           (let ((pos (indice-lista (lambda (x) (eqv? x sym)) syms)))
                             (if (number? pos)
                                 ;Sí está, trae la posicion, y busca esa posicion en la lista de valores que le corresponde
                                 (buscar-pos vals pos)
                                 ;No está, busca de nuevo quitando la parte ya buscada
                                 (buscar-variable env sym))))
      (recursively-extended-env-record (proc-names idss bodies old-env)
                                       (let ((pos (find sym proc-names)))
                                         (if (number? pos)
                                             (cerradura (list-ref idss pos)
                                                      (list-ref bodies pos)
                                                      env)
                                             (buscar-variable old-env sym))))
      )))
 

;; Se define nuevo datatype para la cerradura
(define-datatype procVal procVal?
  (cerradura
   (lista-ID(list-of symbol?))
   (exp expresion?)
   (amb environment?)))


;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procVal proc
      (cerradura (ids body env)
               (evaluar-expresion body (extended-env ids args env))))))


;Función para probar booleanos
(define valor-verdad?
  (lambda(x)
    (not (zero? x))))

;eval-rands
;Evalua los operandos y los convierte en un ambiente
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

;eval-rand
;Ingresa el operando, lo llama para evaluar la expresion
(define eval-rand
  (lambda (rand env)
    (evaluar-expresion rand env)))

;/////////////////////////////////////////////////////////Funciones Auxiliares/////////////////////////////////////////////////////////

(define find
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))


;/////////////////////////////////////////////////////////Resolución de puntos/////////////////////////////////////////////////////////

; PUNTO A
 ;// Funcion areaCirculo
 ;// Calcula el area de un circulo dado un radio (A=PI*r*r)
 ;declarar (@radio=2.5;
 ;          @areaCirculo=procedimiento(@area) haga (3.14*(@area*@area)) finProc)
 ;         { evaluar @areaCirculo(@radio) finEval }
 
 ;declarar (@radio=5@areaCirculo=procedimiento(@area) haga (3.14*(@area*@area)) finProc)
 ;         { evaluar @areaCirculo(@radio) finEval }


; PUNTO B
 ;// Calculo Factorial
 ;// Factorial de 5 y factorial de 10
 ;varFact @factorial(@numero)=
 ;Si @numero entonces (@numero * evaluar@factorial(sub1(@numero))finEval) sino 1 finSI
 ;{evaluar @factorial(10) finEval}
 
 ;varFact @factorial(@numero)=
 ;Si @numero entonces (@numero * evaluar@factorial(sub1(@numero))finEval) sino 1 finSI
 ;{evaluar @factorial(5) finEval}


;
; PUNTO C
; Calcula la suma de dos numeros forma recursiva con las primitivas add1 y sub1. 
;letrec
;       @sumar(@a,@b) = Si @a entonces add1(evaluar @sumar(sub1(@a),@b)finEval) sino @b finSI
;       in
;       evaluar @sumar(4,5) finEval

; PUNTO D
; Resta: calcula la resta de dos numeros de forma recursiva con las primitivas add1 y sub1. 

;letrec
;       @resta(@a,@b) = Si @b entonces sub1(evaluar @resta(@a,sub1(@b))finEval) sino @a finSI
;       in
;       evaluar @resta(10,3) finEval



;Multiplicacion: calcula la multiplicación de dos numeros de forma recursiva con las primitivas add1 y sub1. 

;  letrec
;    @restar(@a,@b) = Si @b entonces evaluar @restar(sub1(@a),sub1(@b)) finEval sino @a finSI
;    @suma(@a,@b) = Si @b entonces evaluar @suma(add1(@a),sub1(@b)) finEval sino @a finSI
;    @multiplicacion(@a,@b) = Si @b entonces evaluar @suma(@a , evaluar @multiplicacion(@a,sub1(@b)) finEval ) finEval sino evaluar @restar(@a,@a) finEval finSI 
;    in evaluar @multiplicacion(10,3) finEval


; PUNTO E

;declaraAlm
;//Procedimiento que retorna un string con los nombres
;@integrantes() = "Marcela-Hassen-Kevin" 
;@hola (@integrantes) = ("Hola:" concat evaluar @integrantes() finEval)
;{
;evaluar @decorate () finEval }

; PUNTO F
;declaraAlm
;//Procedimiento que retorna un string con los nombres
;@integrantes() = "Marcela-Hassen-Kevin"
;@hola(@integrantes) = ("Hola:" concat evaluar @integrantes() finEval) 
;@decorate(@x) = (evaluar @hola(@integrantes) finEval concat @x) 
{
;evaluar @decorate ("-ProfesoresFLP") finEval }


;/////////////////////////////////////////////////////////Dibujos/////////////////////////////////////////////////////////

; PUNTO A
 ;// Funcion areaCirculo
 ;// Calcula el area de un circulo dado un radio (A=PI*r*r)
 declarar (@radio=2.5;
           @areaCirculo=procedimiento(@area) haga (3.14*(@area*@area)) finProc)
          { evaluar @areaCirculo(@radio) finEval }
 
 declarar (@radio=5@areaCirculo=procedimiento(@area) haga (3.14*(@area*@area)) finProc)
          { evaluar @areaCirculo(@radio) finEval }


; PUNTO B
 ;// Calculo Factorial
 ;// Factorial de 5 y factorial de 10
 varFact @factorial(@numero)=
 Si @numero entonces (@numero * evaluar@factorial(sub1(@numero))finEval) sino 1 finSI
 {evaluar @factorial(10) finEval}
 
 varFact @factorial(@numero)=
 Si @numero entonces (@numero * evaluar@factorial(sub1(@numero))finEval) sino 1 finSI
 {evaluar @factorial(5) finEval} 
 