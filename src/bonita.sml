
fun bonita prop =
    	case prop of
        constante false             => "false"
    |   constante true              => "true"
    |   variable nombre             => nombre
    |   negacion prop1              => "~" ^ imprimir  prop1 ^ ")"
    |   conjuncion (prop1, prop2)   => ^ imprimir prop1 ^ " && " ^ imprimir prop2 ^ ")"
    |   disyuncion (prop1, prop2)   =>  ^ imprimir prop1 ^ "  " ^ imprimir prop2 ^ ")"
    |   implicacion (prop1, prop2)  => "implicacion (" ^ imprimir prop1 ^ ", " ^ imprimir prop2 ^ ")"
    |   equivalencia (prop1, prop2) => "equivalencia (" ^ imprimir prop1 ^ ", " ^ imprimir prop2 ^ ")"