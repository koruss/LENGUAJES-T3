(* Lenguaje de proposiciones con constantes. No tiene variables *)

(* Aqui definimos la sintaxis abstracta de nuestro pequenno
   lenguaje de proposiciones con constantes *)

datatype Proposicion =
  constante    of bool
| variable     of string
| negacion     of Proposicion
| conjuncion   of Proposicion * Proposicion
| disyuncion   of Proposicion * Proposicion
| implicacion  of Proposicion * Proposicion
| equivalencia of Proposicion * Proposicion
;

fun imprimir prop =
	case prop of
        constante false             => "false"
    |   constante true              => "true"
    |   variable nombre             => nombre 
    |   negacion prop1              => "negacion (" ^ imprimir  prop1 ^ ")"
    |   conjuncion (prop1, prop2)   => "conjuncion (" ^ imprimir prop1 ^ ", " ^ imprimir prop2 ^ ")" 
    |   disyuncion (prop1, prop2)   => "disyuncion (" ^ imprimir prop1 ^ ", " ^ imprimir prop2 ^ ")"
    |   implicacion (prop1, prop2)  => "implicacion (" ^ imprimir prop1 ^ ", " ^ imprimir prop2 ^ ")"
    |   equivalencia (prop1, prop2) => "equivalencia (" ^ imprimir prop1 ^ ", " ^ imprimir prop2 ^ ")"
;

fun cambiarExpresion prop =
	case prop of
        constante false             => "false"
    |   constante true              => "true"
    |   variable nombre             => nombre 
    |   negacion prop1              => " ~ (" ^ cambiarExpresion  prop1 ^ ")"
    |   conjuncion (prop1, prop2)   => " (" ^cambiarExpresion prop1 ^ " && " ^ cambiarExpresion prop2   ^ ")"
    |   disyuncion (prop1, prop2)   => " (" ^cambiarExpresion prop1 ^ " || " ^ cambiarExpresion prop2 ^ ")"
    |   implicacion (prop1, prop2)  => " (" ^cambiarExpresion prop1 ^ " => " ^ cambiarExpresion prop2 ^ ")"
    |   equivalencia (prop1, prop2) => " (" ^cambiarExpresion prop1 ^ " <=> " ^ cambiarExpresion prop2 ^ ")"
;
 
fun change (prop) = 
	case prop of
        constante false             => "false"
    |   constante true              => "true"
    |   variable nombre             => nombre 
    |   negacion prop1
        => if isConst(prop1) then " ~ " ^ change  prop1 
           else " ~ (" ^ change  prop1 ^ ")"
    |   conjuncion (prop1, prop2)
        => if douConst(prop) then  change  prop1 ^ " && " ^ change  prop2
           else if oneNeg(prop) then  change  prop1 ^ " && " ^ change  prop2
           (*else if oneDouIg(prop) then  change  prop1 ^ " && " ^ change  prop2*)
           else if isConst(prop1) andalso calcPrece(prop) then  change  prop1 ^ " && " ^ change  prop2
           else if isConst(prop1) andalso calcPrece(prop)=false then  change  prop1 ^ " && " ^ "(" ^ change  prop2 ^ ")"
           (*else if oneDouIgNo(prop) then  change  prop1 ^ " && " ^ "(" ^ change  prop2 ^ ")"
           else if douOneIg(prop) then  change  prop1 ^ " && " ^ change  prop2*)
           (*else if oneDouIg(prop)=false then  change  prop1 ^ " && (" ^ change  prop2 ^ ")"*)
           else "( "^ change  prop1 ^ " && " ^ change  prop2 ^ ")"
;

vp & vq
vp & (vp | vr) 
(vp | vr) & vp
(vp | vr) & (vp | vr) 

change(pru1000);

nonfix ~:
val ~: = negacion

infix 7 :&&:
val (op :&&:) = conjuncion

infix 6 :||:
val (op :||:) = disyuncion

infixr 5 :=>:
val (op :=>:) = implicacion

infix 4 :<=>:
val (op :<=>:) = equivalencia

;

val pru1 = (variable "a") :&&: (variable "b") ;
val pru2 = (variable "x") :&&: (variable "y") ;
val pru3 = pru1 :||: pru2 ;
val pru4 = pru3 :=>: pru3 ;

