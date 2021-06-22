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

val pru0 = vp :&&: ~: vp ;

val pru1 = vp :=>: vq ;
val pru2 = t :=>: vq ;
val pru3 = vp :=>: (vq :=>: vq) ;
val pru4 = t :=>: f ;
val pru5 = f :=>: t ;

val pru6 = vp :&&: vq :=>: vq :||: vp ; (* SÍ es una tautología *)
val pru7 = vq :||: vp :=>: vp :&&: vq ; (* NO es una tautología *)
val pru8 = ~: p :&&: p :||: q :&&: ~: q ; (* es una CONTRADICCIÓN *)
val pru9 = ~: vp :&&: vp :||: vq :&&: ~: vq ; (* es una CONTRADICCIÓN *)

(* tautologías triviales, con variables *)
val pru10 = vp :||: ~: vp  :&&: vq :||: ~: vq  (* ojo con la precedencia aquí *)
val pru11 = (vp :||: ~: vp)  :&&: (vq :||: ~: vq)
val pru12 = vp :||: ~: vp  :||: vq :||: ~: vq

(* contradicciones *)
val pru13 = (vp :=>: ~: vp)  :&&: (vq :=>: ~: vq);

val pruSimpl = pru1;



(*Pruebas del equipos utilizadas para la simplificación de las equivalencias lógicas.*)
val vr = variable "r" ;
val vs = constante false;
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