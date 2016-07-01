=begin
*
* Archivo: mod_bfs.rb 
*
* Descripción: Definición del módulo bfs.
* 
* Autores:
*     - Alejandra Cordero / 12-10645
*    - Pablo Maldonado / 12-10561
*    
* Última modificación: 30/06/2016
*
=end

module BFS

	# Descripcion: Algoritmo que realiza BFS sobre objetos y en su recorrido
	#              va ejecutando un bloque de codigo.
    def bfs
        cola = []
        cola.push(self)

        while (cola.size != 0)
            n = cola.shift 
            yield n
            n.each do |hijo|
                cola.push(hijo)
            end
        end
    
    end

    # Descripcion: Dado un bloque de instrucciones de manera explicita 
    #              se recorre una estructura en BFS,invocando a dicho bloque.
    #              Los elementos que cumplan con las condiciones expuestas en 
    #              el mismo seran almacenados en una lista. Esta lista al final 
    #              del recorrido sera retornada y almacenara todos los elementos
    #              que cumplieron con el predicado pasado. 

    def recoger(block) 
        lista = []
        cola = []
        cola.push(self)

        while (cola.size != 0)
            n = cola.shift 
            if block.call(n) == true
                lista = lista + [n]
            end
            n.each do |hijo|
                cola.push(hijo)
            end
        end

        lista
    end

end