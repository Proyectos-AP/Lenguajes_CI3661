=begin
*
* Archivo: trees.rb 
*
* Descripción: Definición de árboles.
* 
* Autores:
*     - Alejandra Cordero / 12-10645
*     - Pablo Maldonado / 12-10561
*    
* Última modificación: 30/06/2016
*
=end

#------------------------------------------------------------------------------#
#                           Inclusión de los módulos                           #
#------------------------------------------------------------------------------#

require_relative "mod_bfs"
require_relative "mod_fold"

#------------------------------------------------------------------------------#
#                   Definición de la clase "Árbol"                             #
#------------------------------------------------------------------------------#

class Arbol

    include BFS
    include DFS

    attr_accessor :valor

    def mutar(claseMutador)
    	@valor = self.valor.mutar(claseMutador)
    end

end

#------------------------------------------------------------------------------#
#                   Definición de la clase "Árbol binario"                     #
#------------------------------------------------------------------------------#

class ArbolBinario < Arbol

    attr_reader :leftChild
    attr_reader :rightChild

    def initialize(val,lChild=nil,rChild=nil) 
        @valor = val 
        @leftChild = lChild
        @rightChild = rChild
    end

    def each 
        [@leftChild,@rightChild].compact.each do |elem|
            yield elem
        end
    end

end

#------------------------------------------------------------------------------#
#                   Definición de la clase "Árbol rosa"                        #
#------------------------------------------------------------------------------#

class ArbolRosa < Arbol

    attr_reader :children

    def initialize(val,*childr) 
        @valor = val 
        @children = childr
    end

    def each
        @children.compact.each do |elem|
            yield elem
        end
    end
end

