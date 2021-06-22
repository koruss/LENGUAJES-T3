fun isConst(prop) = 
	case prop of
    constante (prop) => true
    |   variable nombre => true
		| _ => false
;

fun douConst(prop) = 
  case prop of
    conjuncion (prop1, prop2) => 
        if isConst(prop1) andalso isConst(prop2) then true
        else false
;

fun getNumber(prop) = 
  case prop of
    conjuncion (prop1, prop2)   => 7
    |   disyuncion (prop1, prop2)   => 6
    |   implicacion (prop1, prop2)  => 5
    |   equivalencia (prop1, prop2) => 4
    | _ => 0
;

fun isNeg(prop) =
    case prop of
        negacion prop1 => true
        | _ => false
;

fun oneNeg(prop) = 
    case prop of
        conjuncion (prop1, prop2) => 
            if isNeg(prop1) orelse isNeg(prop2) then true
            else false
;

fun calcPrece (prop) =
    case prop of
        conjuncion (prop1, prop2) => 
            if getNumber(prop)<=getNumber(prop2) then true
            else false
;


fun oneDouIg(prop) = 
    case prop of
    conjuncion (prop1, prop2) => 
        if isConst(prop1) andalso calcPrece(prop) then true
            else false
;

fun oneDouIgNo(prop) = 
    case prop of
    conjuncion (prop1, prop2) => 
        if isConst(propp1) andalso calcPrece(prop)=false then true
            else false
;

fun douOneIg(prop) = 
    case prop of
    conjuncion (prop1, prop2) => 
        if calcPrece(prop) andalso isConst(prop2) then true
            else false
;

