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
		(* else cambiar(ID(prop))  *)
		else cambiar(ID(prop))     (*Implicacion y disyuncion 17*)
		(* :||: cambiar(ID(prop2))  *)
		(* Implicacion Disyuncion *)
	(* cambiar(ID(prop1)) *)
    (* |   implicacion (prop1,prop2)   => ID prop ; cambiar prop1 ; cambiar prop2 *)
	| 	_ =>prop
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
(* 
 *)
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
val xd = test24p2;


val base = vp



(* fun simplificador prop =

	if base <> prop then
		let
		val base =cambiar(prop)
	else base
	simplificador(base)

; *)
fun simplificadorxd(prev, prop) =
	let val prop = cambiar(prop)
	in
	if prev = prop then
		prop
	else simplificadorxd(prop, prop)
	end
;

fun simp(prop) =
	simplificadorxd(prop, prop)
;