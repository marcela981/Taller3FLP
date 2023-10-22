#lang eopl

;;Taller #3

;;Marcela Mazo Castro 1843612


;Especificación Léxica

(define scanner-spec-simple-interpreter
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


;Parte 1 del código diseño de interpretador a la gramática dada.

;Gramatica

(define interprerador-gramatica
  '(

    ;Programa
    (programa (expresion) un-programa)

    ;Expresiones
    (expresion (numero) numero-lit)
    (expresion ("\""texto"\"") texto-lit)
    (expresion (identificador) var-exp)
    (expresion ("("expresion primitiva-binaria expresion")") primapp-bin-exp)
    (expresion (primitiva-unaria "("expresion")") primapp-un-exp)
    (expresion ("Si" expresion "entonces" expresion "sino" expresion "finSI") if-exp)
    (expresion ("declarar" "("  (separated-list identificador "=" expresion ";")   ")" "{" expresion"}") variableLocal-exp)
    (expresion ("evaluar" expresion "("(separated-list expresion ",")")" "finEval") app-exp)

    
    ;Primitiva binaria
    
    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("concat") primitiva-concat)

    ;Primitiva unaria
    
    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)
    
    )
  )


;Parser, Scanner, Interfaz

;Tipos de datos para la sintaxis abstracta ((Análisis léxico y sintáctico)
(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))
  
(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))


;Parte2
;Evaluar el programa


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

;
;Evalua la expresión en el ambiente de entrada
(define eval-expression
  (lambda (exp env)
    (cases expresion exp
      (numero-lit (numero) numero)
      (var-exp (id) (buscar-variable env id))
      (else eopl:error '"No existe dentro de la gramática"))))

;Retorna la posicion en la que el caracter esta ubicado
(define indice-lista
  (lambda (pred lst)
    (indice-lista-aux 0 pred lst)))

; Función auxiliar que realiza el llamado recursivo utilizando como criterio principal
; un contador auxiliar que ayuda a determinar el recorrido dentro de la lista
(define indice-lista-aux
  (lambda (n pred lst)
    (if (null? lst)
        'NoCumple
        (if (pred (car lst))
            n
            (indice-lista-aux (+ n 1) pred (cdr lst))))))

;buscar-pos
;Funcion auxiliar que retorna el elemento que está ubicado en la posicion dada
;(define (buscar-pos lst n)
;  (if (zero? n)
;     (car lst)
;      (buscar-pos (cdr lst) (- n 1))))

;función que busca un símbolo en un ambiente dado
;(define buscar-variable
;  (lambda (env sym)
;    (cases environment env
;      ;No se encontró la variable buscada
;      (empty-env ()
;                   (eopl:error '"Error, la variable no existe"))
      ;busque en la lista de variables si esta la requerida
;     (extended-env (syms vals env)
;                           (let ((pos (indice-lista (lambda (x) (eqv? x sym)) syms)))
;                             (if (number? pos)
;                                 ; en caso si, trae la posicion, y busque esa posicion en la lista de valores que le corresponde
;                                 (buscar-pos vals pos)
                                 ; en caso no, busque de nuevo quitando esa parte ya buscada

;                                 (buscar-variable env sym)))))))