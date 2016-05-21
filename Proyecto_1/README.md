# Implementación de un asistente de pruebas de la lógica proposicional.

Universidad Simón Bolívar  
Departamento de Computación y Tecnología de la Información  
Laboratorio de Lenguajes de Programación I (CI3661)  
Integrantes:  
    Alejandra Cordero / 12-10645  
    Pablo Maldonado / 12-10561  

## Descripción:

* Puede encontrar la descripción de la implementación [aquí](https://github.com/Proyectos-AP/Lenguajes_CI3661/blob/master/Proyecto_1/Enunciado.pdf).

* En ella también encontrará el formato que deben cumplir sus demostraciones para que puedan ser verificadas por el asistente.  

## Instrucciones para correr una prueba:

### Antes de correr una prueba: 
1.- Debe agregar los archivos *Estructuras.hs* y *Funciones.hs* al directorio en el que desee ejecutar su(s) prueba(s).  
2.- Su archivo de teoremas, deberá llamarse *Theorems.hs* y respetar el formato indicado en el enunciado.  
3.- Debe agregarle a *Theorems.hs* el siguiente encabezado:
```haskell
module Theorems (prop) where
import Estructuras
```
4.- Para cada archivo de Teoremas debe agregarle los importes necesarios. Es decir:  
```haskell
import  Estructuras  
import  Funciones 
```
### Para verificar una demostración:

1.- Abra el intérprete de Haskell con el comando:  
```shell
~$ ghci
```  
2.- Cargue el archivo que desea verificar con el comando **:l**. Es decir:  
```haskell
Prelude > :l NombreDeSuArchivo.hs  
```
3.- Desde el intérprete ejecute la función *verify*:  
```haskell
Prelude > verify  
```
4.- Verifique si su prueba fue exitosa o no de acuerdo al mensaje mostrado en pantalla.   



 
