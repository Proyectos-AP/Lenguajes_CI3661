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

	def fold(baseValue,&block)
		puts "Fold"
		baseValue = yield(baseValue,self)
		self.each do |hijo|
			resultado = hijo.fold(baseValue,&block)
			baseValue = resultado
		end
		baseValue
	end
end

=begin
arbolNieto = PinkTree.new(3)
arbolhijo1 = PinkTree.new(1,[arbolNieto])
arbolhijo2 = PinkTree.new(2)
arbolPapa = PinkTree.new(0,[arbolhijo1,arbolhijo2])
=end
