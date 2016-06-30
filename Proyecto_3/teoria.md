# Estudiantes
### 12-10561 - Pablo Maldonado
### 12-10645 - Alejandra Cordero 

# Respuestas

>&nbsp;&nbsp;&nbsp;&nbsp;1. Los lenguajes de programación orientados a objetos que poseen herencia simple están limitados a incorporar comportamientos de un solo ancestro al momento de definir una clase.

>>&nbsp;&nbsp;&nbsp;&nbsp;*No, una de las caracteristicas principales de los lenguajes orientados a objetos es que puede heredar comportamientos de sus ancestros, es decir, las instancias que se heredan no solo se limitan a la de su padre  (que sea ancestro de su ancestro). Cadena*


>&nbsp;&nbsp;&nbsp;&nbsp;2. Lenguajes de POO con un sistemas de tipos estático (C++, Java, C\#) no tienen la posibilidades de elegir la implementación de un método a tiempo de ejecución (despacho dinámico).

>>&nbsp;&nbsp;&nbsp;&nbsp;Estos lenguajes si tiene la posibilidad de elegir que método se eligirá a tiempo de ejecución solo que para hacerlo deben construir una tabla en tiempo de compilacion llamadas virtual table. En las virtual table se colocan todos los métodos y variables que una clase determinada puede alcanzar, es decir, por cada clase se crea una tabla con todos los metodos y variables que esta posee y hereda, de esta forma se permite el despacho dinámico en estos tipos de lenguaje.

>&nbsp;&nbsp;&nbsp;&nbsp;3. La introspección y reflexividad son conceptos que se manejan en la POO pero no guardan ninguna relación entre sí.

>>&nbsp;&nbsp;&nbsp;&nbsp;Respuesta 3

>&nbsp;&nbsp;&nbsp;&nbsp;4. En un lenguaje con un sistema de tipos dinámico la sobrecarga de métodos es innata y representa una comodidad dado que permite implementar un mismo método para distintos tipos.

>>&nbsp;&nbsp;&nbsp;&nbsp;No, existen lenguajes de tipos dinámicos como Python que no sobrecargan los métodos, solo los sobreescribe. Además, por lo general en lenguajes de tipos dinámicos no hace falta sobrecargar porque se pueden usar técnicas como el duck typing debido a la flexibilidad que brinda sus sistema de tipos.

>&nbsp;&nbsp;&nbsp;&nbsp;5. En los lenguajes POO existen los términos interfaz, módulo, clase abstracta, rol, etc; definidos como objetos que pueden encapsular definiciones de clases o implementaciones concretas de métodos.

>>&nbsp;&nbsp;&nbsp;&nbsp;Si, esto es verdad.

>&nbsp;&nbsp;&nbsp;&nbsp;6. Los métodos virtuales permiten asociar, al momento de compilar, una implementación de un método sobrecargado con una llamada del mismo; eliminando el **overhead** del despacho dinámico.

>>&nbsp;&nbsp;&nbsp;&nbsp;No creo que se elimine el overhead porque lo que guaradan las vtables es básicamente las firmas de los métodos, entonces a tiempo de ejecución de igual forma se debería hacer una búsqueda de la firma correspondiente.

>&nbsp;&nbsp;&nbsp;&nbsp;7. Cuando un lenguaje de POO tiene herencia simple no ocurre el problema del diamante pero de igual forma pueden existir llamadas ambiguas de métodos, dado que incorporar interfaces, módulos, protocolos, etc, no evita colisión de nombres.

>>&nbsp;&nbsp;&nbsp;&nbsp;Sí, esto es verdad. ya que supongamos que tenemos una clase C sin método imprimir que hereda de B y esta a su vez hereda de A. Ambas clases tienen el metodo imprimir, entonces, si se define una instancia de C y esta quiere utilizar el metodo antes mencionado habrá un choque de nombre ya que no se sabe si se tiene que utilizar el metodo de A o el de B

>&nbsp;&nbsp;&nbsp;&nbsp;8. El paso de mensaje es un término que se maneja en modelos concurrentes, también de POO y es equivalente a la llamada de una función.

>>&nbsp;&nbsp;&nbsp;&nbsp;Respuesta 8

>&nbsp;&nbsp;&nbsp;&nbsp;9. Sin importar la herencia del lenguaje de POO, una clase podría tener más de un ancestro.

>>&nbsp;&nbsp;&nbsp;&nbsp;Si puede tener más de un ancestro ya que los ancestros de una clase estan conformados por su padre, la clase papa de su padre, la clase papa de su abuelo y asi sucesivamente asi que si una clase esta debajo de una cadena de herencia de mas de dos clases tendra varios ancestros independientemen de si la herencia del lenguaje es simple o multiple.

[comment]: #  "pandoc teoria.md -V geometry:"top=2cm, bottom=1.5cm, left=1cm, right=1cm" --latex-engine=xelatex -o rb_12_10561_12_10645.pdf"


