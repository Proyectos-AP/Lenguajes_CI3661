# Implementación de un verificador de buen etiquetamiento de árboles en Prolog.

Universidad Simón Bolívar  
Departamento de Computación y Tecnología de la Información  
Laboratorio de Lenguajes de Programación I (CI3661)  
Integrantes:  
    Alejandra Cordero / 12-10645  
    Pablo Maldonado / 12-10561  

## Descripción:

* Puede encontrar la descripción de la implementación [aquí](https://github.com/Proyectos-AP/Lenguajes_CI3661/blob/master/Proyecto_2/Enunciado.pdf).
* La implementación utilizada fue [swi-prolog](http://www.swi-prolog.org/).

## Antes de utilizar alguno de los predicados:
1.- Abra su intérprete de comandos y ejecute swi-prolog.
```shell
~$ prolog
``` 
2.- Luego de esto, proceda a cargar el archivo de la base de conocimientos que contiene los predicados.
```prolog
?- [arista].
```
3.- A partir de este momento, podrá utilizar cualquiera de los predicados definidos

### Árboles como listas:

```prolog
	bienEtiquetado(+Arbol).
```

### Árboles como estructuras:

```prolog
	esqueleto(+N,+R,-esqueleto).
```
```prolog
	etiquetamiento(+Esqueleto,-Arbol).
```

```prolog
	esqEtiquetables(+R,+N).
```

```prolog
	describirEtiquetamiento(+Arbol).
```
