(* funcion 100% original homemade vegano gluten free*)
fun cambiar prop =
	case prop of
        constante false             => constante false
    |   constante true              => constante true  
    |   variable nombre             => variable nombre
    |   negacion prop1              
		=> if DNAux(prop) then cambiar(DN(prop)) (* Doble Negacion 1 *)
		else if DMorgAux(prop) then cambiar(DMorg(prop)) (* De Morgan 2,3 *)
		else if isNegComp(prop) then ~:(cambiar(prop1)) (*despiche recursivo por negacion :v*)
		else prop

	|	conjuncion (prop1, prop2) 
		=> if DisAux(prop) then cambiar(Dis(prop)) (*Distributividad 4,5*)
		else if DomAux(prop) then cambiar(Dom(prop)) (* Dominacion 6,7*)
		else if NeAux(prop) then cambiar(Ne(prop)) (*Neutro 8,9 *)	
		else if InvAux(prop) then cambiar(Inv(prop)) (* Inversos 10,11 *)
		else if IdemAux(prop) then cambiar(Idem(prop)) (*Idempotencia 12, 13*)
		else if AbsAux(prop) then cambiar(Abs(prop)) (*Absorcion 14, 15*)
		else cambiar(prop1) :&&: cambiar(prop2)
		
	|	disyuncion (prop1, prop2)
		=> if DisAux(prop) then cambiar(Dis(prop))
		else if DomAux(prop) then cambiar(Dom(prop))
		else if NeAux(prop) then cambiar(Ne(prop))
		else if InvAux(prop) then cambiar(Inv(prop))
		else if IdemAux(prop) then cambiar(Idem(prop))
		else if AbsAux(prop) then cambiar(Abs(prop))
		else cambiar(prop1) :||: cambiar(prop2)

    |   implicacion (prop1,prop2)   
		=> if ExpAux(prop) then cambiar(Exp(prop)) (*Exportacion 16*)
		else cambiar(ID(prop))     (*Implicacion y disyuncion 17*)

	| 	_ => cambiar(prop)
;

(*Funcion recursiva que maneja la mayoría de la carga para simplificar proposiciones lógicas.*)
fun simpRec(prop) = 
	case prop of
	(*Para todas las proposiciones no simplificables, retornar su valor. Esta es
	la condición de parada del algoritmo recursivo.*)
      constante false		=> constante false
    | constante true 		=> constante true  
    | variable nombre		=> variable nombre
		(*La negación es un caso especial que no permite que la recursividad se
		desarrolle completamente. Esto es porque cuando se aplican las reglas de
		DeMorgan y Doble negación, la expresión cambia significativamente. El
		cambio significativo hace que no se puedan simplificar más las
		proposiciones anidadas. Es por esto que existe la funcion simp(prop), la
		cual se asegura de simplificar una expresión lo más posible.*)	
	| negacion prop1	
		=> if DNAux(prop) then simpRec(DN(prop))
		else if DMorgAux(prop) then simpRec(DMorg(prop))
		else if isNegComp(prop) then ~:(simpRec(prop1))
		else prop
		(*Para todos los otros casos de proposiciones compuestas que no son
		negación, se aplica recursividad hasta llegar a la proposición que no se
		puede simplificar.*)
	| conjuncion(prop1, prop2)
		=> if DisAux(prop) then Dis(simpRec(prop1) :&&: simpRec(prop2))
		else if IdemAux(prop) then Idem(simpRec(prop1) :&&: simpRec(prop2))
		else if NeAux(prop) then Ne(simpRec(prop1) :&&: simpRec(prop2))
		else if InvAux(prop) then Inv(simpRec(prop1) :&&: simpRec(prop2))
		else if DomAux(prop) then Dom(simpRec(prop1) :&&: simpRec(prop2))
		else if AbsAux(prop) then Abs(simpRec(prop1) :&&: simpRec(prop2))
		else simpRec(prop1) :&&: simpRec(prop2)
	| disyuncion(prop1, prop2)
		=> if DisAux(prop) then Dis(simpRec(prop1) :||: simpRec(prop2))
		else if IdemAux(prop) then Idem(simpRec(prop1) :||: simpRec(prop2))
		else if NeAux(prop) then Ne(simpRec(prop1) :||: simpRec(prop2))
		else if InvAux(prop) then Inv(simpRec(prop1) :||: simpRec(prop2))
		else if DomAux(prop) then Dom(simpRec(prop1) :||: simpRec(prop2))
		else if AbsAux(prop) then Abs(simpRec(prop1) :||: simpRec(prop2))
		else simpRec(prop1) :||: simpRec(prop2)
	| implicacion(prop1, prop2)
		=> if ExpAux(prop) then Exp(simpRec(prop1) :=>: simpRec(prop2))
		else ID(simpRec(prop1) :=>: simpRec(prop2))
	| _ => prop
;

(*Función que se encarga de llamar a la función simplificadora recursiva hasta
que la expresión ya no sea simplificable.*)
fun simpMax(prev, prop) =
	let val prop = simpRec(prop)
	in
	(*Si la expresión es igual a la de la ejecución anterior, ya no es
	simplificable, por lo tanto se retorna.*)
	if prev = prop then
		prop
	else simpMax(prop, prop)
	end
;

fun simp(prop) =
	simpMax(prop, prop)
;

fun debugProp(prop) = 
	if DNAux(prop) then "DobleNeg" (* Doble Negacion 1 *)
	else if DMorgAux(prop) then "DMorg" (* De Morgan 2,3 *)
	else if isNegComp(prop) then "Negacion Comp" (*despiche recursivo por negacion :v*)
	else if DisAux(prop) then "Distributiva" (*Distributividad 4,5*)
	else if DomAux(prop) then "Dominacion" (* Dominacion 6,7*)
	else if NeAux(prop) then "Neutro" (*Neutro 8,9 *)	
	else if InvAux(prop) then "Inversos" (* Inversos 10,11 *)
	else if IdemAux(prop) then "Idempotencia" (*Idempotencia 12, 13*)
	else if AbsAux(prop) then "Absorcion"(*Absorcion 14, 15*)
	else if ExpAux(prop) then "Exportacion"(*Exportacion 16*)
	else "ID o llamada recursiva"
;
val dbg = debugProp;
(* http://calculator-online.org/mathlogic *)
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
