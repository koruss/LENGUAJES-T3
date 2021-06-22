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
    |  disyuncion (prop1, prop2) => 
        if isConst(prop1) andalso isConst(prop2) then true
        else false 
    | _ => false
;

fun getNumber(prop) = 
  case prop of
    negacion prop1                  => 8
    |   conjuncion (prop1, prop2)   => 7
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
        |   disyuncion (prop1, prop2) => 
                if isNeg(prop1) orelse isNeg(prop2) then true
                else false  
        | _ => false   
;

fun calcPrece (prop) =
    case prop of
        conjuncion (prop1, prop2) => 
            if getNumber(prop)<=getNumber(prop2) then true
            else false
        |    disyuncion (prop1, prop2) => 
                if getNumber(prop)<=getNumber(prop2) then true
                else false
        | _ => false         
;

fun calcPreceIzq (prop) =
    case prop of
        conjuncion (prop1, prop2) => 
            if getNumber(prop1)>=getNumber(prop) then true
            else false
        |    disyuncion (prop1, prop2) => 
                if getNumber(prop1)>=getNumber(prop) then true
                else false
        | _ => false         
;


fun oneDouIg(prop) = 
    case prop of
    conjuncion (prop1, prop2) => 
        (* if isConst(prop1) andalso calcPrece(prop) then true *)
        if calcPrece(prop) then true
            else false
    |   disyuncion (prop1, prop2) => 
            (* if isConst(prop1) andalso calcPrece(prop) then true *)
            if calcPrece(prop) then true
                else false
    | _ => false         
;

fun DouOneIg(prop) = 
    case prop of
    conjuncion (prop1, prop2) => 
        (* if isConst(prop2) andalso calcPreceIzq(prop) then true *)
        if calcPreceIzq(prop) then true
            else false
    |   disyuncion (prop1, prop2) => 
            (* if isConst(prop2) andalso calcPreceIzq(prop) then true *)
            if calcPreceIzq(prop) then true
                else false
    | _ => false         
;

fun oneO (prop) = 
    case prop of 
        conjuncion (prop1, prop2) => 
            if oneDouIg(prop) then  change  prop1 ^ " && " ^ change  prop2
            else change  prop1 ^ " && (" ^ change  prop2 ^ ")"
        |   disyuncion (prop1, prop2) => 
                if oneDouIg(prop) then  change  prop1 ^ " || " ^ change  prop2
                else change  prop1 ^ " || (" ^ change  prop2 ^ ")"
        | _ => "no hay"
;

fun oneT (prop) = 
    case prop of 
        conjuncion (prop1, prop2) => 
            if DouOneIg(prop) then  change  prop1 ^ " &&l " ^ change  prop2
            else "("^change  prop1 ^ ") && " ^ change  prop2 
        |   disyuncion (prop1, prop2) => 
                if DouOneIg(prop) then  change  prop1 ^ " || " ^ change  prop2
                else "( "^change  prop1 ^ ") || " ^ change  prop2 
        | _ => "no hay"
;

(* (vp :&&: vq) :&&: (vq :&&: vp) = vp :&&: vq :&&: vq :&&: vp *)
(* (vp :||: vq) :&&: (vq :&&: vp) = (vp :||: vq) :&&: vq :&&: vp *)
(* (vp :&&: vq) :&&: (vq :||: vp) = vp :&&: vq :&&: (vq :||: vp) *)
(* (vp :||: vq) :&&: (vq :||: vp) = (vp :||: vq) :&&: (vq :||: vp) *)

fun double(prop) = 
    case prop of 
        conjuncion(prop1, prop2) =>
            if DouOneIg(prop) andalso oneDouIg(prop) then change  prop1 ^ " && " ^ change  prop2
            else if DouOneIg(prop)=false andalso oneDouIg(prop) then "("^change  prop1 ^ ") && " ^ change  prop2
            else if DouOneIg(prop) andalso oneDouIg(prop)=false then change  prop1 ^ " && (" ^ change  prop2^")"
            else "("^change  prop1 ^ ") && (" ^ change  prop2^")"
        |   disyuncion(prop1, prop2) =>
                if DouOneIg(prop) andalso oneDouIg(prop) then change  prop1 ^ " && " ^ change  prop2
                else if DouOneIg(prop)=false andalso oneDouIg(prop) then "("^change  prop1 ^ ") && " ^ change  prop2
                else if DouOneIg(prop) andalso oneDouIg(prop)=false then change  prop1 ^ " && (" ^ change  prop2^")"
                else "("^change  prop1 ^ ") && (" ^ change  prop2^")"
        | _ => "no hay"
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
           else if isConst(prop1) then oneO(prop)
           else if isConst(prop2) then oneT(prop)
           else double(prop)
           (* else if oneDouIg(prop) then  change  prop1 ^ " && " ^ change  prop2 *)
           (*else if isConst(prop1) andalso calcPrece(prop) then  change  prop1 ^ " && " ^ change  prop2
           else if isConst(prop1) andalso calcPrece(prop)=false then  change  prop1 ^ " && (" ^ change  prop2 ^ ")"*)
           (*else if oneDouIgNo(prop) then  change  prop1 ^ " && " ^ "(" ^ change  prop2 ^ ")"
           else if douOneIg(prop) then  change  prop1 ^ " && " ^ change  prop2*)
           (*else if oneDouIg(prop)=false then  change  prop1 ^ " && (" ^ change  prop2 ^ ")"*)
           (* else "( "^ change  prop1 ^ " && " ^ change  prop2 ^ ")" *)
    |   disyuncion (prop1, prop2)
        => if douConst(prop) then  change  prop1 ^ " || " ^ change  prop2        
           else if oneNeg(prop) then  change  prop1 ^ " || " ^ change  prop2
           else if isConst(prop1) then oneO(prop)
           else if isConst(prop2) then oneT(prop)
           else double(prop)
           (* else "( "^ change  prop1 ^ " || " ^ change  prop2 ^ ")"  *)
    | _ => "no hay"
;