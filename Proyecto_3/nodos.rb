=begin
*
* Archivo: nodos.rb 
*
* Descripción: Definición de los nodos y mutadores.
* 
* Autores:
*     - Alejandra Cordero / 12-10645
*     - Pablo Maldonado / 12-10561
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

    def self.int_to_array(int_val)
        lista = []
        digito = int_val % 10
        lista = lista + [digito]
        cociente = int_val / 10
        while (cociente > 0)
            lista = [(cociente % 10)] + lista 
             cociente = cociente / 10 
        end
        lista
    end
end

class Singular < Mutador

     def self.mut_fixnum(int_val)
         # Descripcion: int_val se muta a la multiplicación del ultimo dígito 
         # con la suma de los dígitos restantes.
         puts "Fixnum Singular"
         lista = self.int_to_array(int_val)
         ultimoDigito = lista.pop
         if lista.size == 0 then
         	acum = 1
         else
			acum = 0
		end
		
         lista.each do |elem|
             acum = acum + elem
         end
         ultimoDigito*acum
     end

    def self.mut_str(str_val)
        # Descripcion: los caracteres de str_val se convierten a mayuscula si
        # alguno de ellos pertenece a "singular"
        puts "Str Singular"
        new = ""
        str_val.each_char do |c|
            if "singular".include?(c)
                 c = c.upcase
             end
             new = new + c
         end
        new
     end

     def self.mut_array(array_val)
         # Descripcion: todos los elementos de array_val se interpolan en un
         # string separados por un espacio
         puts "Array Singular"
         lista = array_val.flatten
         resultado = lista[0].to_s
         lista.shift
         lista.each do |c|
             resultado = resultado + " " + c.to_s
         end
         resultado
     end

end

class Uniforme < Mutador

    def self.mut_fixnum(int_val)
        # Descripcion: int_val se convierte en el promedio de sus digitos.
         puts "Fixnum Uniforme"
         lista = self.int_to_array(int_val)
         numElems = lista.size
         acum = 0
         lista.each do |elem|
             acum = acum + elem
         end
         acum.to_f / numElems
     end

     def self.mut_str(str_val)
         # Descripcion: str_val se muta y sus valores se convierten en mayuscula
         # de forma intescalada.
         puts "Str Uniforme"
         new = ""
         i = 0
         str_val.each_char do |c|
              if i % 2 == 0 
                  c = c.upcase
              end
              new = new + c
             i = i + 1
         end
         new
     end

     def self.mut_array(array_val)
         # Descripcion: los elementos de array_val se mutan de manera uniforme.
         puts "Array Uniforme"
         lista = []
         array_val.each do |c|
             lista = lista + [c.mutar(Uniforme)]
         end
         lista
     end

end
 
class Oscuro < Mutador

    def self.mut_fixnum(int_val)
        # Descripcion: int_val se muta de forma que se elminaran los digitos
        # en la posicion impar.
         puts "Fixnum Oscuro"
         lista = self.int_to_array(int_val)
         i = 0
         acum = ""
         lista.each do |elem|
             if (i % 2 == 0) then
                 acum = acum + elem.to_s
             end
             i = i + 1
         end
         acum.to_i
     end

     def self.mut_str(str_val)
         # Descripcion: str_val se muta de forma que quedaran los caracteres 
         # en posición impar concatenados del lado izquierdo y los otros 
         # concatenados del lado derecho
         puts "Str Oscuro"
         charImpar = ""
         charPar = ""
         i = 0
         str_val.each_char do |c|

              if i % 2 != 0 then
                  charImpar = charImpar + c 
              else
                  charPar = charPar + c 
              end
              i = i + 1
         end
         charImpar + charPar  
     end

     def self.mut_array(array_val)
         # Descripcion: se seleccionan 50% de los elementos de array_val y 
         # se mutan de forma oscura.
         puts "Array Oscuro"
         longitud = array_val.size
         listaR = Array.new(longitud) {|i| i}
         lista = listaR.shuffle
         numElems = lista.size
         mitad = numElems / 2
         lista = lista.take(mitad)

         lista.each do |c|
             array_val[c] = array_val[c].mutar(Oscuro)
         end
         
         array_val
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
        mut_class.mut_array(self)
    end 
end