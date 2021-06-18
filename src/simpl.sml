(* funcion 100% original homemade vegano gluten free*)
fun cambiar prop =
	case prop of
        constante false             => constante false
    |   constante true              => constante true  
    |   variable nombre             => variable nombre
    |   negacion prop1              
		=> if DNAux(prop) then cambiar(DN(prop)) (* Doble Negacion 1 *)
		else if DMorgAux(prop) then cambiar(DMorg(prop)) (* De Morgan 2,3 *)
		else if isNegComp(prop) then cambiar(prop1)
		else prop

	|	conjuncion (prop1, prop2) 
		=> if DomAux(prop) then cambiar(Dom(prop)) (* Dominacion 4,5*)
		else if NeAux(prop) then cambiar(Ne(prop)) (*Neutro 6,7 *)	
		else if InvAux(prop) then cambiar(Inv(prop)) (* Inversos 8,9 *)
		else if IdemAux(prop) then cambiar(Idem(prop)) (*Idempotencia 10*)
		else if AbsAux(prop) then cambiar(Abs(prop)) (*Absorcion 11, 12*)
		else cambiar(prop1) :&&: cambiar(prop2)
		
	|	disyuncion (prop1, prop2)
		=> if DomAux(prop) then cambiar(Dom(prop))
		else if NeAux(prop) then cambiar(Ne(prop))
		else if InvAux(prop) then cambiar(Inv(prop))
		else if IdemAux(prop) then cambiar(Idem(prop))
		else if AbsAux(prop) then cambiar(Abs(prop))
		else cambiar(prop1) :||: cambiar(prop2)

    |   implicacion (prop1,prop2)   
		=> if ExpAux(prop) then cambiar(Exp(prop))
		(* else cambiar(ID(prop))  *)
		else cambiar(ID(prop)) 
		(* :||: cambiar(ID(prop2))  *)
		(* Implicacion Disyuncion *)
	(* cambiar(ID(prop1)) *)
    (* |   implicacion (prop1,prop2)   => ID prop ; cambiar prop1 ; cambiar prop2 *)
	| 	_ =>prop
;
(* http://calculator-online.org/mathlogic *)
val vr = variable "r" ;
val vs = constante false;
(* val lmao = vp :||: vs;
val dm = ~:(vp :||: vq);
val ab = vp :||: (vp :&&: vq);
val ac = (vp :||: vq) :&&: vp ; *)
val xdd = (vp :=>: vq) :=>: vr;
val xp = vp :=>: (vq :=>: vr);