=begin
*
* Archivo: trees.rb 
*
* Descripción: Definición de árboles.
* 
* Autores:
* 	- Alejandra Cordero / 12-10645
* 	- Pablo Maldonado / 12-10561
*	
* Última modificación: 30/06/2016
*
=end

#------------------------------------------------------------------------------#
#                           Inclusión de los módulos                           #
#------------------------------------------------------------------------------#

require_relative 'mod_bfs'
require_relative 'mod_fold'

#------------------------------------------------------------------------------#
#                   Definición de la clase "Árbol binario"                     #
#------------------------------------------------------------------------------#

# lo logico seria que haya un Tree del que hereden Binary y Pink (pendiente)

class BinaryTree
	include BFS
	include DFS

	attr_accessor :node
	attr_reader :leftChild
	attr_reader :rightChild

	def initialize(val,lChild=nil,rChild=nil) 
		@node = val 
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

class PinkTree

	include BFS
	include DFS

	attr_accessor :node
	attr_reader :children

	def initialize(val,childr = []) 
		@node = val 
		@children = childr
	end

	# No se si lo que esta arriba se puede poner en una clase general de arbo.
	def each
		@children.compact.each do |elem|
			yield elem
		end
	end
end

arbol1 = BinaryTree.new(1)

arbol2 = BinaryTree.new(2)

arbol3 = BinaryTree.new(2,arbol1,arbol2)

valor = arbol3.node()
puts "El valor del arbol3 es #{valor}"
arbol2.node = 3
valor = arbol1.node()
puts "El valor del arbol1 es #{valor}"

hijoIzq = arbol3.leftChild()

valorIzq = hijoIzq.node()
puts "El valor del nodo del hijo izq es #{valorIzq}"

arbol3.each do |child|
	puts "hola #{child.node}"
end