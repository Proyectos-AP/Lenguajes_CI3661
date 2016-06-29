=begin
*
* Archivo: mod_fold.rb 
*
* Descripción: Definición del módulo fold.	
* 
* Autores:
* 	- Alejandra Cordero / 12-10645
* 	- Pablo Maldonado / 12-10561
*	
* Última modificación: 30/06/2016
*
=end

module DFS

	def dfs

		puts "DFS"
		puts "dfs #{self.node}"
		self.each do |hijo|
			hijo.dfs
		end

	end

	def fold 
		puts "Fold"
	end
end