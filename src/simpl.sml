(* funcion 100% original homemade vegano gluten free*)
(*Función recursiva que maneja la mayoría de la carga para simplificar proposiciones lógicas.*)
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
  let val prop1 = simpRec(prop)
  in
  (*Si la expresión es igual a la de la ejecución anterior, ya no es
  simplificable, por lo tanto se retorna.*)
    if prop = prop1 then
      prop
    else simpMax(prop, prop1)
  end
;

(*Función principal a ser llamada para simplificar una proposición.*)
fun simp(prop) =
	simpMax(prop, prop)
;

(*Función utilizada para depurar el proceso de simplificación, se ingresa un
proposición y retorna la regla de simplificación que se supone que va a aplicar
SimpRec.*)
fun debugProp(prop) = 
	if DNAux(prop) then "DobleNeg" (* Doble Negacion 1 *)
	else if DMorgAux(prop) then "DMorg" (* De Morgan 2,3 *)
	else if isNegComp(prop) then "Negacion Comp" 
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