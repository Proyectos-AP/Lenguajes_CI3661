Implementación de un asistente de pruebas de la lógica proposicional.

Universidad Simón Bolívar  
Departamento de Computación y Tecnología de la Información  
Laboratorio de Lenguajes de Programación I (CI3661)  
Integrantes:  
    Alejandra Cordero / 12-10645  
    Pablo Maldonado / 12-10561  

 Descripción:

* Puede encontrar la descripción de la implementación en el documento Enunciado.pdf .
* En ella también encontrará el formato que deben cumplir sus demostraciones para que puedan ser verificadas por el asistente.  

 Instrucciones para correr una prueba:

1. Debe agregar los archivos Estructuras.hs y Funciones.hs al directorio en el que desee ejecutar su(s) prueba(s).  

2. En su archivo de teoremas (Theorems.hs) debe agregar el siguiente encabezado:

module Theorems (prop) where
  
import Estructuras

Con este encabezado se hacen los importens necesarios para el correcto funcionamiento
de Theorems.hs y ademas se permite tratar el archivo de teoremas como un modulo.

3. Para cada archivo de Teoremas deberá agregarle los importes necesarios. Es decir:  

import  Estructuras  
import  Funciones 

