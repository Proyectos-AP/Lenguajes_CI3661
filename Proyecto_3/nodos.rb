=begin
*
* Archivo: nodos.rb 
*
* Descripción: Definición de los nodos y mutadores.
* 
* Autores:
* 	- Alejandra Cordero / 12-10645
* 	- Pablo Maldonado / 12-10561
*	
* Última modificación: 30/06/2016
*
=end

#------------------------------------------------------------------------------#
#                   Definición de la clase "Nodo"                              #
#------------------------------------------------------------------------------#

class Nodo
	attr_accessor :element
end

#------------------------------------------------------------------------------#
#                     Definición de los Mutadores                              #
#------------------------------------------------------------------------------#

=begin
class Mutador
end

class Singular < Mutador
end

class Uniforme < Mutador
end
 
class Oscuro < Mutador
end
=end 
#------------------------------------------------------------------------------#
#                        Redefinición de clases                                #
#------------------------------------------------------------------------------#

class ::String
	def singular 
		new = ""
		self.each_char do |c|
			if "singular".include?(c)
				c = c.upcase
			end
			new = new + c
		end
		return new
 	end

 	def uniforme
 		new = ""
 		i = 0
 		self.each_char do |c|
 			if i % 2 == 0 
 				c = c.upcase
 			end
 			new = new + c
 			i = i + 1
 		end
 		return new
 	end

end


=begin 
class String
end 

class Array
end
=end
