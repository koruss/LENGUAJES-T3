(*Probar si una negación tiene expresiones posiblemente simplificables adentro.*)
fun isNegComp(prop) = 
	case prop of
		(*En caso de ser una negacion compuesta.*)
		negacion (disyuncion(prop1, prop2)) => true
		|negacion (conjuncion(prop1, prop2)) => true
		|negacion (implicacion(prop1, prop2)) => true
		|negacion (equivalencia(prop1, prop2)) => true
		(*En caso de no ser negacion compuesta, retornar falso*)
		| _ => false
;

(*Probar si a una negacion se le puede aplicar la regla de doble negacion.*)
fun DNAux(prop) =
	case prop of
		negacion (negacion prop1) => true
		(*En caso de no ser doble negacion, retornar false*)
		| _ => false
;

(*Doble negacion*)
(*Antes de correr la función de doble negación se debe probar si a la expresión
se le puede aplicar la regla, utilicando DNAux.*)
fun DN(prop) = 
	case prop of
		(*En caso de ser doble negacion, IDificar*)
		negacion (negacion prop1) => prop1 (* ¬¬P ≡ P *)
		(*En caso de no ser doble negacion, no hacer nada*)
		| _ => prop
;

(*Probar si a una conjuncion o una disyuncion se le puede aplicar la regla de Idempotencia.*)
fun IdemAux (prop)=
	case prop of
		disyuncion (prop1, prop2)
			=> if prop1 = prop2 then true 
				else false
    	|conjuncion (prop1, prop2)  
			=> if prop1 = prop2 then true
				else false
		|_ => false
;

(*Idempotencia*)
(*Antes de correr la función de idempotencia se debe probar si a la expresión
se le puede aplicar la regla, utilicando IdemAux.*)
fun Idem (prop) =
	case prop of
    conjuncion(prop1,prop2)=>prop2
    | disyuncion(prop1, prop2) => prop2
	|_ => prop
;

(*Probar si a una conjuncion o disyuncion se le puede aplicar regla de inversos.*)
fun InvAux(prop) = 
	case prop of
		disyuncion (prop1, prop2)
			=> if prop1 = DN(~:prop2) then true (*utiliza doble negación para asegurarse que la propocisiones siempre van a ser equivalentes.*)
				else false
    	|conjuncion (prop1, prop2)  
			=> if prop1 = DN(~:prop2) then true
				else false
		|_ => false
;

(*Inversos*)
(*Antes de correr la función de inversos se debe probar si a la expresión
se le puede aplicar la regla, utilicando InvAux.*)
fun Inv(prop) = 
	case prop of
		disyuncion (prop1, prop2) => constante true
    	|conjuncion (prop1, prop2) => constante false
		|_ => prop
;

(*Probar si a una conjuncion o disyuncion se le puede aplicar regla de neutro.*)
fun NeAux(prop) =
	case prop of
		 disyuncion (prop1, prop2)
			=> if prop1 = constante false orelse prop2 = constante false then true
				else false
		| conjuncion (prop1, prop2)
			=> if prop1 = constante true orelse prop2 = constante true then true 
				else false
		| _ => false
;

(*Neutro*)
(*Antes de correr la función de Neutro se debe probar si a la expresión
se le puede aplicar la regla, utilicando NeAux.*)
fun Ne(prop) =
	case prop of
		disyuncion (prop1, prop2) 
			=> if prop1 = constante false then prop2 (*  P ∨ F0 ≡ P *)
				else prop1
		| conjuncion (prop1, prop2) (* P ∧ V0 ≡ P *)
			=> if prop1 = constante true then prop2
				else prop1
		| _ => prop
;

(*Probar si a una conjuncion o disyuncion se le puede aplicar regla de dominacion.*)
fun DomAux(prop) =
	case prop of
		disyuncion (prop1, prop2)
			=> if prop1 = constante true orelse prop2 = constante true then true 
				else false
    	|conjuncion (prop1, prop2)  
			=> if prop1 = constante false orelse prop2 = constante false then true
				else false
		|_ => false
;

(*Dominacion*)
(*Antes de correr la función de Dominación se debe probar si a la expresión
se le puede aplicar la regla, utilicando DomAux.*)
fun Dom(prop) =
	case prop of
		disyuncion (prop1, prop2) => constante true (*  P ∨ V ≡ V *)
		| conjuncion (prop1, prop2) => constante false	(* P ∧ F0 ≡ F0 *)
		| _ => prop
;

(*Probar si a una conjuncion o disyuncion se le puede aplicar regla de De Morgan.*)
fun DMorgAux(prop) =
	case prop of
		 negacion(disyuncion(prop1,prop2)) => true
		| negacion(conjuncion(prop1,prop2)) => true
		| 	_ => false
;

(*DeMorgan*)
(*Antes de correr la función de DeMorgan se debe probar si a la expresión
se le puede aplicar la regla, utilicando DMorgAux.*)
fun DMorg(prop) = 
	case prop of
		 negacion(disyuncion(prop1,prop2)) => ~:prop1 :&&: ~:prop2 (* ¬(P ∨ Q) ≡ ¬P ∧ ¬Q *)
		| negacion(conjuncion(prop1,prop2)) => ~:prop1 :||: ~:prop2(* ¬(P ∧ Q) ≡ ¬P ∨ ¬Q *)
		| 	_ => prop
;

(* Implicacion y disyuncion *)
fun ID prop =
	case prop of
    	implicacion (prop1, prop2)  => disyuncion(negacion prop1, prop2)
		| 	_ => prop
;

(*Probar si a una conjuncion o disyuncion se le puede aplicar regla de Absorcion.*)
fun AbsAux prop =
	case prop of
		disyuncion(conjuncion(prop1, prop2), prop3) 
			=> if prop1 = prop3 orelse prop2 = prop3 then true 
				else false
		| disyuncion(prop1, conjuncion(prop2, prop3)) 
			=> if prop1 = prop2 orelse prop1 = prop3 then true 
				else false
		| conjuncion(disyuncion(prop1, prop2), prop3) 
			=> if prop1 = prop3 orelse prop2 = prop3 then true 
				else false
		| conjuncion(prop1, disyuncion(prop2, prop3)) 
			=> if prop1 = prop2 orelse prop1 = prop3 then true 
				else false
		|_ => false
;

(*Absorcion*)
(*Antes de correr la función de Absorcion se debe probar si a la expresión
se le puede aplicar la regla, utilicando AbsAux.*)
fun Abs(prop) =
	case prop of
		disyuncion(conjuncion(prop1, prop2), prop3) => prop3
		| disyuncion(prop1, conjuncion(prop2, prop3)) => prop1
		| conjuncion(disyuncion(prop1, prop2), prop3) => prop3
		| conjuncion(prop1, disyuncion(prop2, prop3)) => prop1
		|_ => prop
;

(*Probar si a una implicacion se le puede aplicar la regla de exportacion.*)
fun ExpAux(prop) = 
	case prop of
		implicacion(prop1, implicacion(prop2, prop3)) 
			=> if prop1 <> prop2 andalso prop2 <> prop3 andalso prop1<>prop3 then true 
			else false
		| _ => false
;

(*Exportación*)
(*Antes de correr la función de Exportación se debe probar si a la expresión
se le puede aplicar la regla, utilicando ExpAux.*)
fun Exp(prop) = 
	case prop of
		implicacion(prop1, implicacion(prop2, prop3)) => (prop1 :&&: prop2) :=>: prop3
		| _ => prop
;

(*Probar si a una expresión se le puede aplicar la regla de distributividad.*)
fun DisAux(prop) = 
	case prop of
		conjuncion(disyuncion(prop1, prop2), disyuncion(prop3, prop4)) 
			=> if prop1 = prop3 andalso prop1 <> prop2 andalso prop1 <> prop4 
			orelse prop1 = prop4 andalso prop1 <> prop2 andalso prop1 <> prop3 
			orelse prop2 = prop3 andalso prop2 <> prop1 andalso prop2 <> prop4
			orelse prop2 = prop4 andalso prop2 <> prop1 andalso prop2 <> prop3
				then true 
				else false
		| disyuncion(conjuncion(prop1, prop2), conjuncion(prop3, prop4)) 
			=> if prop1 = prop3 andalso prop1 <> prop2 andalso prop1 <> prop4 
			orelse prop1 = prop4 andalso prop1 <> prop2 andalso prop1 <> prop3 
			orelse prop2 = prop3 andalso prop2 <> prop1 andalso prop2 <> prop4
			orelse prop2 = prop4 andalso prop2 <> prop1 andalso prop2 <> prop3
				then true 
				else false
		| _ => false
;

(*Distributividad*)
(*Antes de correr la función de Distributividad se debe probar si a la expresión
se le puede aplicar la regla, utilicando DisAux.*)
fun Dis(prop) =
	case prop of
		conjuncion(disyuncion(prop1, prop2), disyuncion(prop3, prop4)) 
			=> if prop1 = prop3 then
				prop1 :||: (prop2 :&&: prop4)
			else if prop1 = prop4 then
				prop1 :||: (prop2 :&&: prop3)
			else if prop2 = prop3 then
				prop2 :||: (prop1 :&&: prop4)
			else if prop2 = prop4 then
				prop2 :||: (prop1 :&&: prop3)
			else prop
		| disyuncion(conjuncion(prop1, prop2), conjuncion(prop3, prop4)) 
			=> if prop1 = prop3 then
				prop1 :&&: (prop2 :||: prop4)
			else if prop1 = prop4 then
				prop1 :&&: (prop2 :||: prop3)
			else if prop2 = prop3 then
				prop2 :&&: (prop1 :||: prop4)
			else if prop2 = prop4 then
				prop2 :&&: (prop1 :||: prop3)
			else prop
		| _ => prop
;
