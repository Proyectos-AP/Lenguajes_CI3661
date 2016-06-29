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

class Mutador
end

class Singular < Mutador

 	def self.mut_fixnum(int_val)
 		# Descripcion: int_val se muta a la multiplicación del ultimo dígito 
 		# con la suma de los dígitos restantes.
 		puts "Fixnum Singular"
 	end

	def self.mut_str(str_val)
		# Descripcion: los caracteres de str_val se convierten a mayuscula si
		# alguno de ellos pertenece a "singular"
		puts "Str Singular"
		# new = ""
		# self.each_char do |c|
		# 	if "singular".include?(c)
		# 		c = c.upcase
		# 	end
		# 	new = new + c
		# end
		# return new
 	end

 	def self.mut_array(array_val)
 		# Descripcion: todos los elementos de array_val se interpolan en un
 		# string separados por un espacio
 		puts "Array Singular"
 	end

end

class Uniforme < Mutador

	def self.mut_fixnum(int_val)
		# Descripcion: int_val se convierte en el promedio de sus digitos.
 		puts "Fixnum Uniforme"
 	end

 	def self.mut_str(str_val)
 		# Descripcion: str_val se muta y sus valores se convierten en mayuscula
 		# de forma intescalada.
 		puts "Str Uniforme"
 		# =begin
 		# new = ""
 		# i = 0
 		# self.each_char do |c|
 		# 	if i % 2 == 0 
 		# 		c = c.upcase
 		# 	end
 		# 	new = new + c
 		# 	i = i + 1
 		# end
 		# return new
 	end

 	def self.mut_array(array_val)
 		# Descripcion: los elementos de array_val se mutan de manera uniforme.
 		puts "Array Uniforme"
 	end

end
 
class Oscuro < Mutador

	def self.mut_fixnum(int_val)
		# Descripcion: int_val se muta de forma que se elminaran los digitos
		# en la posicion impar.
 		puts "Fixnum Oscuro"
 	end

 	def self.mut_str(str_val)
 		# Descripcion: str_val se muta de forma que quedaran los caracteres 
 		# en posición impar concatenados del lado izquierdo y los otros 
 		# concatenados del lado derecho
 		puts "Str Oscuro"
 	end

 	def self.mut_array(array_val)
 		# Descripcion: se seleccionan 50% de los elementos de array_val y 
 		# se mutan de forma oscura.
 		puts "Array Oscuro"
 	end

end

#------------------------------------------------------------------------------#
#                        Redefinición de clases                                #
#------------------------------------------------------------------------------#

class ::Fixnum
	def mutar(mut_class)
		mut_class.mut_fixnum(self)	
	end
end

#------------------------------------------------------------------------------#

class ::String
	def mutar(mut_class)
		mut_class.mut_str(self)
	end
end

class ::Array 
	def mutar(mut_class)
		mut_class.mut_str(self)
	end 
end