# Implementación de un verificador de buen etiquetamiento de árboles en Prolog.

Universidad Simón Bolívar  
Departamento de Computación y Tecnología de la Información  
Laboratorio de Lenguajes de Programación I (CI3661)  
Integrantes:  
    Alejandra Cordero / 12-10645  
    Pablo Maldonado / 12-10561  

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
* Si desea verificar el buen etiquetamiento de un árbol, use la función *bienEtiquetado(+Arbol)/1*
```prolog
	% Ejemplo:
	bienEtiquetado(nodo(4,[arista(1,nodo(3,[])),arista(2,nodo(2,[])),arista(3,nodo(1,[]))])).
```
   y el intérprete dará la respuesta correspondiente:
``` prolog
    ?- true.
```

## Árboles como estructuras:
*
```prolog
	esqueleto(+N,+R,-esqueleto).
```
*
```prolog
	etiquetamiento(+Esqueleto,-Arbol).
```
*
```prolog
	esqEtiquetables(+R,+N).
```
* Si desea imprimir en un 
```prolog
	describirEtiquetamiento(+Arbol).
```
