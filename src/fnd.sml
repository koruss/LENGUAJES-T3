

fun fnd prop =
    let
    	(* variables de trabajo *)
        val resultado = ""
    	val variables = vars prop
    	val n = length variables
    	val lista_combinaciones_booleanas = gen_bools n
    	(* imprimir una fila de la tabla de verdad *)
    	fun imprimir_fila vars_bools es_verdadero =
            if es_verdadero then print ( imprimirFnd (vars_bools,n) ^ "||" )
            else print("")
    	(* generar evaluaciones de la proposición*)
    	fun recorrer []                  = print "\n"  (* toque final a la impresión; previamente mostramos hileras con el resultado *)
		|   recorrer (fila :: mas_filas) = 
    		    let
		        	(* establecer una asociación entre variables y una combinación de valores booleanos (fila) *)
                    val asociacion = as_vals variables fila
                    (* esta asociación constituye un ambiente, o contexto, para evaluar la proposición prop *)
 	    		    val resultado_fila = evalProp asociacion prop
                in
            	    imprimir_fila  asociacion  resultado_fila (* efecto: imprimir fila y su evaluación *)
            	    ;
            	    recorrer mas_filas (* continuar el trabajo *)
            	end
    in
        recorrer lista_combinaciones_booleanas
    end
;