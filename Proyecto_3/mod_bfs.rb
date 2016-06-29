=begin
*
* Archivo: mod_bfs.rb 
*
* Descripción: Definición del módulo bfs.
* 
* Autores:
* 	- Alejandra Cordero / 12-10645
*	- Pablo Maldonado / 12-10561
*	
* Última modificación: 30/06/2016
*
=end

module BFS

	def bfs
		puts "BFS"
		puts "soy self #{self}"
		cola = []
		cola.push(self)

		while (cola.size != 0)
			puts "hola"
			n = cola.shift 
			yield n
			n.each do |hijo|
				cola.push(hijo)
			end
		end
	
	end

	def recoger(&block) 
		puts "Recoger"	
		block.call 
	end

end