(* Entrada: Recibe un preposición a evaluar
   Salida:  Devuelve un valor booleano con el resultado de la consulta
   Revisa si la proposición que entra es una constante*)
fun isConst(prop) = 
	case prop of
    constante (prop) => true
    |   variable nombre => true
		| _ => false
;

(* Entrada: Recibe un preposición a evaluar
   Salida:  Devuelve un valor booleano con el resultado de la consulta
   Revisa si la porposición tiene constantes como valor final*)
fun douConst(prop) = 
  case prop of
    conjuncion (prop1, prop2) => 
        if isConst(prop1) andalso isConst(prop2) then true
        else false
    |  disyuncion (prop1, prop2) => 
        if isConst(prop1) andalso isConst(prop2) then true
        else false 
    |  implicacion (prop1, prop2) => 
        if isConst(prop1) andalso isConst(prop2) then true
        else false 
    |  equivalencia (prop1, prop2) => 
        if isConst(prop1) andalso isConst(prop2) then true
        else false 
    | _ => false
;

(* Entrada: Recibe un preposición a evaluar
   Salida:  Devuelve un valor con el resultado de la consulta
   Revisa cuál es la precedencia de cada una de las expresiones, con 
   el fin de poder devolver el valor de cada uno para una comparación*)
fun getNumber(prop) = 
  case prop of
    negacion prop1                  => 8
    |   conjuncion (prop1, prop2)   => 7
    |   disyuncion (prop1, prop2)   => 6
    |   implicacion (prop1, prop2)  => 5
    |   equivalencia (prop1, prop2) => 4
    | _ => 0
;


(* Entrada: Recibe un preposición a evaluar
   Salida:  Devuelve un valor booleano con el resultado de la consulta
   Revisa si la porposición que ingresa esta compuesta con una negación*)
fun isNeg(prop) =
    case prop of
        negacion prop1 => true
        | _ => false
;


(* Entrada: Recibe un preposición a evaluar
   Salida:  Devuelve un valor booleano con el resultado de la consulta
   Revisa si una de las subproposiciones es negativa*)
fun oneNeg(prop) = 
    case prop of
        conjuncion (prop1, prop2) => 
            if isNeg(prop1) orelse isNeg(prop2) then true
            else false
        |   disyuncion (prop1, prop2) => 
                if isNeg(prop1) orelse isNeg(prop2) then true
                else false  
        |   implicacion (prop1, prop2) => 
                if isNeg(prop1) orelse isNeg(prop2) then true
                else false  
        |   equivalencia (prop1, prop2) => 
                if isNeg(prop1) orelse isNeg(prop2) then true
                else false  
        | _ => false   
;


(* Entrada: Recibe un preposición a evaluar
   Salida:  Devuelve un valor booleano con el resultado de la consulta
   Calcula la diferencia entre los valores numericos de cada expresión
   usando como referencia la subproposición derecha*)
fun calcPrece (prop) =
    case prop of
        conjuncion (prop1, prop2) => 
            if getNumber(prop)<=getNumber(prop2) then true
            else false
        |    disyuncion (prop1, prop2) => 
                if getNumber(prop)<=getNumber(prop2) then true
                else false
        |    implicacion (prop1, prop2) => 
                if getNumber(prop)<=getNumber(prop2) then true
                else false
        |    equivalencia (prop1, prop2) => 
                if getNumber(prop)<=getNumber(prop2) then true
                else false
        | _ => false         
;

(* Entrada: Recibe un preposición a evaluar
   Salida:  Devuelve un valor booleano con el resultado de la consulta
   Calcula la diferencia entre los valores numericos de cada expresión
   usando como referencia la subproposición izquierda*)
fun calcPreceIzq (prop) =
    case prop of
        conjuncion (prop1, prop2) => 
            if getNumber(prop1)>=getNumber(prop) then true
            else false
        |    disyuncion (prop1, prop2) => 
                if getNumber(prop1)>=getNumber(prop) then true
                else false
        |    implicacion (prop1, prop2) => 
                if getNumber(prop1)>=getNumber(prop) then true
                else false
        |    equivalencia (prop1, prop2) => 
                if getNumber(prop1)>=getNumber(prop) then true
                else false
        | _ => false         
;

(* Entrada: Recibe un preposición a evaluar
   Salida:  Devuelve un valor booleano con el resultado de la consulta
   Dada una preposición compuesta, hace llamado a funciones necesarias
   para revisar la precedencia de las operaciones*)
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
    |   implicacion (prop1, prop2) => 
            (* if isConst(prop1) andalso calcPrece(prop) then true *)
            if calcPrece(prop) then true
                else false
    |   equivalencia (prop1, prop2) => 
            (* if isConst(prop1) andalso calcPrece(prop) then true *)
            if calcPrece(prop) then true
                else false
    | _ => false         
;

(* Entrada: Recibe un preposición a evaluar
   Salida:  Devuelve un valor booleano con el resultado de la consulta
   Dada una preposición compuesta, hace llamado a funciones necesarias
   para revisar la precedencia de las operaciones*)
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
    |   implicacion (prop1, prop2) => 
            (* if isConst(prop2) andalso calcPreceIzq(prop) then true *)
            if calcPreceIzq(prop) then true
                else false
    |   equivalencia (prop1, prop2) => 
            (* if isConst(prop2) andalso calcPreceIzq(prop) then true *)
            if calcPreceIzq(prop) then true
                else false
    | _ => false         
;

(* Entrada: Recibe un preposición a evaluar
   Salida:  Devuelve un valor string con el resultado de la consulta
   Es una subfunción que va armando el resultado de la operación 
   basandose en que la proposición entrante tenga un valor único a 
   la izquierda*)
fun oneO (prop) = 
    case prop of 
        conjuncion (prop1, prop2) => 
            if oneDouIg(prop) then  change  prop1 ^ " && " ^ change  prop2
            else change  prop1 ^ " && (" ^ change  prop2 ^ ")"
        |   disyuncion (prop1, prop2) => 
                if oneDouIg(prop) then  change  prop1 ^ " || " ^ change  prop2
                else change  prop1 ^ " || (" ^ change  prop2 ^ ")"
        |   implicacion (prop1, prop2) => 
                if oneDouIg(prop) then  change  prop1 ^ " => " ^ change  prop2
                else change  prop1 ^ " => (" ^ change  prop2 ^ ")"
        |   equivalencia (prop1, prop2) => 
                if oneDouIg(prop) then  change  prop1 ^ " <=> " ^ change  prop2
                else change  prop1 ^ " <=> (" ^ change  prop2 ^ ")"
        | _ => "no hay"
;

(* Entrada: Recibe un preposición a evaluar
   Salida:  Devuelve un valor string con el resultado de la consulta
   Es una subfunción que va armando el resultado de la operación 
   basandose en que la proposición entrante tenga un valor único a 
   la derecha*)
fun oneT (prop) = 
    case prop of 
        conjuncion (prop1, prop2) => 
            if DouOneIg(prop) then  change  prop1 ^ " &&l " ^ change  prop2
            else "("^change  prop1 ^ ") && " ^ change  prop2 
        |   disyuncion (prop1, prop2) => 
                if DouOneIg(prop) then  change  prop1 ^ " ||1 " ^ change  prop2
                else "( "^change  prop1 ^ ") || " ^ change  prop2 
        |   implicacion (prop1, prop2) => 
                if DouOneIg(prop) then  change  prop1 ^ " => " ^ change  prop2
                else "( "^change  prop1 ^ ") => " ^ change  prop2 
        |   equivalencia (prop1, prop2) => 
                if DouOneIg(prop) then  change  prop1 ^ " <=> " ^ change  prop2
                else "( "^change  prop1 ^ ") <=> " ^ change  prop2 
        | _ => "no hay"
;

(* (vp :&&: vq) :&&: (vq :&&: vp) = vp :&&: vq :&&: vq :&&: vp *)
(* (vp :||: vq) :&&: (vq :&&: vp) = (vp :||: vq) :&&: vq :&&: vp *)
(* (vp :&&: vq) :&&: (vq :||: vp) = vp :&&: vq :&&: (vq :||: vp) *)
(* (vp :||: vq) :&&: (vq :||: vp) = (vp :||: vq) :&&: (vq :||: vp) *)


(* Entrada: Recibe un preposición a evaluar
   Salida:  Devuelve un valor string con el resultado de la consulta
   Es una subfunción que va armando el resultado de la operación 
   basandose en que la proposición entrante tenga preposiciones 
   compuestas como subpreposiciones*)
fun double(prop) = 
    case prop of 
        conjuncion(prop1, prop2) =>
            if DouOneIg(prop) andalso oneDouIg(prop) then change  prop1 ^ " && " ^ change  prop2
            else if DouOneIg(prop)=false andalso oneDouIg(prop) then "("^change  prop1 ^ ") && " ^ change  prop2
            else if DouOneIg(prop) andalso oneT(prop)=false then change  prop1 ^ " && (" ^ change  prop2^")"
            else "("^change  prop1 ^ ") &&6 (" ^ change  prop2^")"
        |   disyuncion(prop1, prop2) =>
                if DouOneIg(prop) andalso oneDouIg(prop) then change  prop1 ^ " || " ^ change  prop2
                else if DouOneIg(prop)=false andalso oneDouIg(prop) then "("^change  prop1 ^ ") || " ^ change  prop2
                else if DouOneIg(prop) andalso oneDouIg(prop)=false then change  prop1 ^ " || (" ^ change  prop2^")"
                else "("^change  prop1 ^ ") || (" ^ change  prop2^")"
        |   implicacion(prop1, prop2) =>
                if DouOneIg(prop) andalso oneDouIg(prop) then change  prop1 ^ " => " ^ change  prop2
                else if DouOneIg(prop)=false andalso oneDouIg(prop) then "("^change  prop1 ^ ") => " ^ change  prop2
                else if DouOneIg(prop) andalso oneDouIg(prop)=false then change  prop1 ^ " => (" ^ change  prop2^")"
                else "("^change  prop1 ^ ") => (" ^ change  prop2^")"
        |   equivalencia(prop1, prop2) =>
                if DouOneIg(prop) andalso oneDouIg(prop) then change  prop1 ^ " <=> " ^ change  prop2
                else if DouOneIg(prop)=false andalso oneDouIg(prop) then "("^change  prop1 ^ ") <=> " ^ change  prop2
                else if DouOneIg(prop) andalso oneDouIg(prop)=false then change  prop1 ^ " <=> (" ^ change  prop2^")"
                else "("^change  prop1 ^ ") <=>0 (" ^ change  prop2^")"
        | _ => "no hay"
;

(* Entrada: Recibe un preposición a evaluar
   Salida:  Devuelve un valor string con el resultado de la consulta
   Es una subfunción que va armando el resultado de la operación 
   basandose en que la proposición entrante tenga un valor negativo 
   y le siga una preposición compuesta*)
fun revNegs(prop) = 
    case prop of
        conjuncion (prop1, prop2)
            => if isConst(prop1) andalso isConst(prop2) then  change  prop1 ^ " &&14" ^ change  prop2
            else if isNeg(prop1) andalso oneDouIg(prop) then change  prop1 ^ " &&15" ^ change  prop2
            else if isNeg(prop2) andalso DouOneIg(prop) then "("^change  prop1 ^ ") &&16" ^ change  prop
            else  "("^change  prop1 ^ ") &&20 (" ^ change  prop2^")"
        |   disyuncion (prop1, prop2)
                => if isConst(prop1) andalso isConst(prop2) then  change  prop1 ^ " || " ^ change  prop2
                else if isNeg(prop1) andalso oneDouIg(prop) then change  prop1 ^ " ||17 ~(" ^ change  prop2^")"
                else if isNeg(prop2) andalso DouOneIg(prop) then "("^change  prop1 ^ ") ||19 " ^ change  prop2
                else  "("^change  prop1 ^ ") ||16 (" ^ change  prop2^")"
        |   implicacion (prop1, prop2)
                => if isConst(prop1) andalso isConst(prop2) then  change  prop1 ^ " => " ^ change  prop2
                else if isNeg(prop1) andalso oneDouIg(prop) then change  prop1 ^ " => ~(" ^ change  prop2^")"
                else if isNeg(prop2) andalso DouOneIg(prop) then "("^change  prop1 ^ ") => " ^ change  prop2
                else  "("^change  prop1 ^ ") =>16 (" ^ change  prop2^")"
        |   equivalencia (prop1, prop2)
                => if isConst(prop1) andalso isConst(prop2) then  change  prop1 ^ " <=>18 " ^ change  prop2
                else if isNeg(prop1) andalso oneDouIg(prop) then change  prop1 ^ " <=>17 ~(" ^ change  prop2^")"
                else if isNeg(prop2) andalso DouOneIg(prop) then "("^change  prop1 ^ ") <=>19 " ^ change  prop2
                else  "("^change  prop1 ^ ") <=>16 (" ^ change  prop2^")"
        | _ => "no hay"
;


(* Entrada: Recibe un preposición a evaluar
   Salida:  Devuelve un valor string con el resultado de la consulta
   Función principal en bonita la cual irá evaluando cada preposición
   para obtener un resultado tipo string el cual será una expresión
   lógica con únicamente los parentesis que son necesarios para que 
   la operación cumpla con todo lo requerido*)
fun change (prop) = 
	case prop of
        constante false             => "false"
    |   constante true              => "true"
    |   variable nombre             => nombre 
    |   negacion prop1
        => if isConst(prop1) then " ~ " ^ change  prop1 
           else " ~ (" ^ change  prop1 ^ ")"
    |   conjuncion (prop1, prop2)
        => if douConst(prop) then  change  prop1 ^ " &&11 " ^ change  prop2
           else if oneNeg(prop) then  revNegs(prop)
           else if isConst(prop1) then oneO(prop)
           else if isConst(prop2) then oneT(prop)
           else double(prop)
    |   disyuncion (prop1, prop2)
        => if douConst(prop) then  change  prop1 ^ " || " ^ change  prop2        
           else if oneNeg(prop) then  revNegs(prop)
           else if isConst(prop1) then oneO(prop)
           else if isConst(prop2) then oneT(prop)
           else double(prop)
    |   implicacion (prop1, prop2)
        => if douConst(prop) then  change  prop1 ^ " => " ^ change  prop2        
           else if oneNeg(prop) then  revNegs(prop)
           else if isConst(prop1) then oneO(prop)
           else if isConst(prop2) then oneT(prop)
           else double(prop)
    |   equivalencia (prop1, prop2)
        => if douConst(prop) then  change  prop1 ^ " <=> " ^ change  prop2        
           else if oneNeg(prop) then  revNegs(prop)
           else if isConst(prop1) then oneO(prop)
           else if isConst(prop2) then oneT(prop)
           else double(prop)
    | _ => "no hay"
;