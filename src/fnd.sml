
(* Funcion principal encargada de evaluar la proposicion asignada y retornar 
  la forma normal disyuntiva (FND) de la misma*)
fun fnd prop =
    let
    	(* variables de trabajo *)
        val resultado = ""
    	val variables = vars prop
    	val n = length variables
        val cont = 1
        val cont2 = ref 0
    	val lista_combinaciones_booleanas = gen_bools n
    	(* Se encarga de unir e imprimir el resultado de los conjuntos de variables separados por un 'O' *)
    	fun imprimir_fila vars_bools es_verdadero =
 
            if es_verdadero then 
                if !cont2 = 0 then 
                    print("(" ^ imprimirFnd (vars_bools,n,cont)^ " || " )
                else
                    print(" || (" ^ imprimirFnd (vars_bools,n,cont))
            else  cont2 := !cont2 +  1 
  
    	(* generar evaluaciones de la proposición*)
    	fun recorrer []                  = print "\n"  (* toque final a la impresión; previamente mostramos hileras con el resultado *)
		|   recorrer (fila :: mas_filas) = 
    		    let
		        	(* establecer una asociación entre variables y una combinación de valores booleanos (fila) *)
                    val asociacion = as_vals variables fila
                    (* esta asociación constituye un ambiente, o contexto, para evaluar la proposición prop *)
 	    		    val resultado_fila = evalProp asociacion prop
                in
                    (* efecto: imprimir fila y su evaluación *)
            	    imprimir_fila  asociacion  resultado_fila 
            	    ;
            	    recorrer mas_filas (* continuar el trabajo *)
            	end
    in
        recorrer lista_combinaciones_booleanas
    end
;