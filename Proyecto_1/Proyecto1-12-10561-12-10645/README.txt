Implementación de un asistente de pruebas de la lógica proposicional.

Universidad Simón Bolívar  
Departamento de Computación y Tecnología de la Información  
Laboratorio de Lenguajes de Programación I (CI3661)  
Integrantes:  
    Alejandra Cordero / 12-10645  
    Pablo Maldonado / 12-10561  

 Descripción:

* Puede encontrar la descripción de la implementación en el documento Enunciado.pdf.
* En ella también encontrará el formato que deben cumplir sus demostraciones para que puedan ser verificadas por el asistente.  

 Instrucciones para correr una prueba:

 - Antes de correr una prueba:

   1. Debe agregar los archivos Estructuras.hs y Funciones.hs al directorio en el que desee ejecutar su(s) prueba(s).  
   2.- Su archivo de teoremas, deberá llamarse Theorems.hs y tener el siguiente encabezado:

    module Theorems (prop) where
  
    import Estructuras

  Con este encabezado, se hacen los importens necesarios para el correcto funcionamiento
  de Theorems.hs; y además, se permite tratar el archivo de teoremas como un módulo.

   3. Para cada archivo de Teoremas deberá agregarle los importes necesarios. Es decir:  

    import  Estructuras  
    mport  Funciones 

- Para verificar una demostración:

  1.- Abra el intérprete de Haskell con el comando:

	~$ ghci

  2.- Cargue el archivo que desea verificar con el comando :l. Es decir:

	Prelude > :l NombreDeSuArchivo.hs  

  3.- Desde el intérprete ejecute la función verify:

	Prelude > verify  

  4.- Verifique si su prueba fue exitosa o no de acuerdo al mensaje mostrado en pantalla.

