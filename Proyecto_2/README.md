# Implementación de un verificador de buen etiquetamiento de árboles en Prolog.

Universidad Simón Bolívar  
Departamento de Computación y Tecnología de la Información  
Laboratorio de Lenguajes de Programación I (CI3661)  
Integrantes:  
&nbsp;&nbsp;&nbsp;&nbsp;Alejandra Cordero / 12-10645  
&nbsp;&nbsp;&nbsp;&nbsp;Pablo Maldonado / 12-10561  

## Descripción:

* Puede encontrar la descripción de la implementación [aquí](https://github.com/Proyectos-AP/Lenguajes_CI3661/blob/master/Proyecto_2/Enunciado.pdf).
* La implementación de Prolog utilizada para el desarrollo fue [swi-prolog](http://www.swi-prolog.org/), versión 6.6.

## Antes de utilizar alguno de los predicados:
1.- Abra su intérprete de comandos y ejecute el intérprete de *swi-prolog*.
```shell
~$ prolog
``` 
2.- Luego de esto, proceda a cargar el archivo de la base de conocimientos que contiene los predicados.
```prolog
?- [arista].
```
3.- A partir de este momento, podrá utilizar cualquiera de los predicados definidos.

## Árboles como listas: 
* Si desea verificar el buen etiquetamiento de un árbol, use el predicado *bienEtiquetado(+Arbol)/1*
```prolog
	% Ejemplo:
	bienEtiquetado(nodo(4,[arista(1,nodo(3,[])),arista(2,nodo(2,[])),arista(3,nodo(1,[]))])).
```
&nbsp;&nbsp;&nbsp;&nbsp;y el intérprete dará la respuesta correspondiente:
``` prolog
    ?- true.
```

## Árboles como estructuras:
* En caso de que desee generar todos los árboles *R-arios* con *N nodos (N > 0)*, use el predicado *esqueleto(+N,+R,-esqueleto)*
```prolog
	% Ejemplo:
	esqueleto(3,2,Esqueleto).
```
&nbsp;&nbsp;&nbsp;&nbsp;y el intérprete dará la respuesta correspondiente:
``` prolog
    ?- esqueleto(3,5,Esqueleto).
    Esqueleto = [[1], [1], [0]] ;
    Esqueleto = [[2], [0, 0]] .
```

* Si desea obtener árboles bien etiquetados a partir de un esqueleto, utilice el predicado *etiquetamiento(+Esqueleto,-Arbol)*
```prolog
	etiquetamiento(+Esqueleto,-Arbol).
```
* En caso de que se desee verificar si todos los esqueletos de árboles *R-arios* con *N nodos* son bien etiquetables, use el predicado *esqEtiquetables(+R,+N)*

```prolog
	% Ejemplo:
	esqEtiquetables(3,5).
```
&nbsp;&nbsp;&nbsp;&nbsp;y el intérprete dará la respuesta correspondiente:
``` prolog
    ?- true.
```
* Si desea mostrar en pantalla un árbol en un formato más sencillo de leer, use el predicado *describirEtiquetamiento(+Arbol).*
```prolog
	% Ejemplo:
	describirEtiquetamiento(nodo(4,[arista(1,nodo(3,[])),arista(2,nodo(2,[])),arista(3,nodo(1,[]))])).
```
&nbsp;&nbsp;&nbsp;&nbsp;y el intérprete dará la respuesta correspondiente:
``` prolog
    nodo(4
         [arista(1
                 nodo(3
                      []))
         arista(2
                 nodo(2
                      []))
         arista(3
                 nodo(1
                      []))
         ])
    true 


```
