(*Pruebas realizadas por el profesor*)
(* pruebas con constantes *)

val f = constante false
val t = constante true

val prop1 = f :=>: f :<=>: ~: f :=>: ~: f
val prop2 = f :=>: f :<=>: ~: f :||: f
;

val p = f;
val q = t;

val prop3 = p :=>: q :<=>: ~: p :||: q
val prop4 = p :=>: q :<=>: ~: q :=>: ~: p
;

(* pruebas con variables *)

val vp = variable "p" ;
val vq = variable "q" ;

val vr = variable "r" ;
val vm = variable "m" ;

val pru0 = vp :&&: ~: vp ;

val pru1 = vp :=>: vq ;

val pru2 = t :=>: vq ;

val pru3 = vp :=>: (vq :=>: vp);
val pru4 = t :=>: f ;
val pru5 = f :=>: t ;

val pru6 = vp :&&: vq :=>: vr :||: vm ; (* SÍ es una tautología *)
val pru7 = vr :||: vp :=>: vp :&&: vq ; (* NO es una tautología *)
val pru8 = ~: p :&&: p :||: q :&&: ~: q ; (* es una CONTRADICCIÓN *)
val pru9 = ~: vp :&&: vp :||: vq :&&: ~: vm ; (* es una CONTRADICCIÓN *)

(* tautologías triviales, con variables *)

val pru10 = vp :||: ~: vr  :&&: vq :||: ~: vq  


val pru11 = (vp :||: ~: vp)  :&&: (vq :||: ~: vq)
val pru12 = vp :||: ~: vp  :||: vq :||: ~: vq

(* contradicciones *)
val pru13 = (vp :=>: ~: vp)  :&&: (vq :=>: ~: vq);

val pruSimpl = pru1;

(*Pruebas realizadas por los estudiantes*)
val vr = variable "r" ;
val vs = constante false;

(*Pruebas para la función vars:*)

val pruVars1 = vr :=>: ((vp :||: ~: vp)  :&&: (vp :=>: ~: vq));

(*Pruebas para la función gen_bools*)

val pruGenBools1 = 5;
val pruGenBools2 = 2;

(*Pruebas para la función as_vals*)

val pruAV_Vars = ["p", "q", "r"];
val pruAV_Bools = [true, false, true];

(*Pruebas para la función eval_prop*)

val asociacion1 = as_vals pruAV_Vars pruAV_Bools;

(*Pruebas para la función taut*)
val taut_test = taut pruVars1;

(*Pruebas para la simplificación de las equivalencias lógicas.*)
(* val lmao = vp :||: vs;
val dm = ~:(vp :||: vq);
val ab = vp :||: (vp :&&: vq);
val ac = (vp :||: vq) :&&: vp ; *)
val xp = vp :=>: (vq :=>: vr);
(* Debugging *)
(* val xdd = (vp :=>: vq) :=>: vr; *)

val dist = (vp :||: vq) :&&: (vp :||: vr);
val test24 = (vp :||: vq) :&&: ~:(~:vp :&&: vq);
val test24p2 = ~:(vq :&&: (vr :||: vq)) :&&: (vp :||: ~:vq);
val t24 = test24 :||: test24p2;
val t22 = vp :||: ((vq :&&: vr) :||: (~:vq :&&: vr));
val xd = test24p2;

val db1 = negacion(conjuncion(variable "q", disyuncion(variable "r", variable "q")));
val db2 = disyuncion(variable "p", negacion(variable "q"));
val db3 = db1 :&&: db2;

val base = vp

(*Ejemplos hecho por Murillo.*)
val ej1 = vp :||: (~:(~:vr:||:vp) :||: vr);
val ej2 = (vp :||: (~:(~:vr:||:vp))) :||: vr;
val ej3 = (vp :||: (~:(~: vq :||: ~: vr)) :||: ~:(~: vq :=>: ~: vr));