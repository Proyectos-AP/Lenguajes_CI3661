/*
 * Archivo: arista.pl
 *
 * Descripción: Implementación de predicados que permiten verificar 
 * etiquetamientos de un árbol, generar esqueletos, etiquetarlos, e 
 * imprimir dicha estructura de forma legible para el usuario.
 *
 * Autores:
 *     - Alejandra Cordero / 12-10645
 *     - Pablo Maldonado / 12-10561
 *
 * Última fecha de modificación: 16/06/2016
 *
*/

%------------------------------------------------------------------------------%
%                        DEFINICIÓN DE ESTRUCTURAS                             %
%------------------------------------------------------------------------------%

%% arista/2: Define la estructura aristas con las condiciones dadas. Es decir,
%% posee una etiqueta (entero positivo) y un subárbol asociado.
%%     + X : Etiqueta asociada al nodo.
%%     + A : Árbol asociado al nodo.
arista(X,A) :- 
	integer(X),
	X > 0,
	arbol(A).

%------------------------------------------------------------------------------%

%% arbol/2: Define la estructura de un nodo con las condiciones establecidas.
%% Es decir, posee una etiqueta (entero positivo) y una lista de aristas
%% asociada.

%% Caso en el que es una hoja (Lista vacía):  
%%     + X : Etiqueta asociada al nodo.
arbol(nodo(X,[])) :- 
	integer(X),
	X > 0.

%% Caso en el que el nodo tiene al menos un hijo en su lista de aristas.
%%     + X : Etiqueta asociada al nodo.
arbol(nodo(X,_)) :-
	integer(X),
	X > 0.

%------------------------------------------------------------------------------%

%% entero/1: preficado que verifica si una lista es de enteros.

%% Caso base de la recursión. En la lista vacía se cumple la condición
entero([]).

%% Se verifica que un elemento en particular sea un entero.
entero(X) :- integer(X).

%% Caso recursivo en el que se verifica el primer elemento de la lista, y luego
%% la cola de la misma.
entero([H|T]):- integer(H), entero(T).

%------------------------------------------------------------------------------%

%% esq/1: verifica que una lista corresponda a la estructura de esqueleto.

%% Caso base, el esqueleto vacío.
esq([]).

%% Caso recursivo, se verifica que la cabeza de la lista sea una lista de 
%% enteros y que su cola también lo sea. 
esq([H|T]) :-
	entero(H),
	esq(T).

%------------------------------------------------------------------------------%

%% lista/3: este predicado verifica que tanto las etiquetas de las aristas
%% como la de los nodos no se repitan entre sí. 

%% Caso base de la inicialización de las listas:
lista([],[],[]).

%% Caso en el que se  está revisando una arista.
%%     + X : Etiqueta de la arista.
%%     + Y : Etiqueta del nodo asociado a la arista.
%%     + A : Lista de aristas asociadas al nodo.
%%     - Ar : Lista de las etiquetas de las aristas del árbol.
%%     - N : Lista de las etiquetas de los nodos del árbol. 
lista(arista(X,nodo(Y,A)),Ar,N):-
	lista(A,R1,R2),
	\+ member(X,R1),
	Ar = [X|R1],
	N =  [Y|R2].

%% En este caso, se recibe una lista de aristas, y se hacen las llamadas 
%% recursivas correspondientes:
%%     + H : Cabeza de la lista de aristas.
%%     + T : Cola de la lista de aristas
%%     - A : Lista de las etiquetas de las aristas del árbol.
%%     - N : Lista de las etiquetas de los nodos del árbol.
%%     * A1 : Construcción parcial de las etiquetas de aristas del árbol (H)
%%     * N1 : Construcción parcial de las etiquetas de nodos del árbol (H)
%%     * A2 : Construcción parcial de las etiquetas de aristas del árbol (T)
%%     * N2 : Construcción parcial de las etiquetas de nodos del árbol (H)
lista([H|T],A,N) :-
	lista(H,A1,N1),
	lista(T,A2,N2),
	append(A1,A2,A),
	is_set(A),
	append(N1,N2,N),
	is_set(N).

%------------------------------------------------------------------------------%
%              VERIFICACIÓN DE BUEN ETIQUETAMIENDO DE UN ÁRBOL                 %
%------------------------------------------------------------------------------%

%% bienEtiquetado/2: este predicado permite verificar si un árbol está bien 
%% etiquetado. Para ello, se verifica la buena construcción y cumplimiento
%% de los rangos establecidos para las mismas.

%% Caso base para verificar que un nodo esté bien etiquetado. En caso de que 
%% esto suceda, su etiqueta debe ser igual a uno (1).
bienEtiquetado(nodo(1,[])).

%% Caso en el que existe más de un nodo. Se verifica el buen etiquetamiento
%% con la función auxiliar bienEtiquetadoR/3, y luego se verifican que todas
%% las etiquetas (tanto de nodos como de aristas) cumplan con el rango 
%% establecido (etiquetasNodos -> {1,..,N}, etiquetasAristas -> {1,..,N-1}) 
%%     + X : Etiqueta asociada al nodo.
%%     + H : Cabeza de la lista de aristas.
%%     + T : Cola de la lista de aristas
%%     * A : Lista de las etiquetas de las aristas del árbol.
%%     * N : Lista de las etiquetas de los nodos del árbol.
%%     * NR : Lista de las etiquetas de los nodos del árbol que incluye a x.
%%     * LN : Número de nodos del árbol.
%%     * LA : Número de aristas del árbol.
bienEtiquetado(nodo(X,[H|T])) :-
	bienEtiquetadoR(nodo(X,[H|T]),A,N),
	append(N,[X],NR),
	length(NR,LN),
	LA is LN - 1,
	etiquetas_validas(A,LA),
	etiquetas_validas(NR,LN).

%------------------------------------------------------------------------------%

%% bienEtiquetadoR/3: predicado auxiliar para la verificación del buen 
%% etiquetamiento de un árbol.

%% Caso base de la recursión en el que un nodo no tiene aristas asociadas, 
%% por lo que no hace falta verificar ningún etiquetamiento.
bienEtiquetadoR(nodo(_,[]),_,_).

%% Caso en el que si existen aristas asociadas al nodo. Para cada una de ellas
%% se verifica que el etiquetamiento satisfaga la condición establecida. Luego,
%% se verifica que las etiquetas de aristas y nosos no se repitan, y se hace 
%% la llamada recursiva.
%%     + X : Etiqueta asociada al nodo.
%%     + H : Cabeza de la lista de aristas.
%%     + T : Cola de la lista de aristas
%%     - A : Lista de las etiquetas de las aristas del árbol.
%%     - N : Lista de las etiquetas de los nodos del árbol.
%%     * Arist : Etiqueta asociada a la arista.
%%     * Nod : Etiqueta asociada al nodo (inferior)
%%     * Etiqueta : Se utiliza para la verificación del buen etiquetamiento.
bienEtiquetadoR(nodo(X,[H|T]),A,N) :-
	buscarA(H,Arist,Nod),
	Etiqueta is abs(X-Nod),
	Etiqueta == Arist,
	lista([H|T],A,N),
	\+ member(X,N),
	bienEtiquetadoR(nodo(X,T),_,_).

%------------------------------------------------------------------------------%

%% buscarA/3: predicado para verificar el buen etiquetamiento del nodo asociado
%% a una arista y obtener los valores de las etiquetas de la arista y del nodo
%% (inferior).
%%     +/- X : Etiqueta asociada a la arista.
%%     +/- Y : Etiqueta asociada al nodo inferior.
%%     +/- A : Lista de aristas del nodo.
buscarA(arista(X,nodo(Y,A)),X,Y) :-
	bienEtiquetadoR(nodo(Y,A),_,_).

%------------------------------------------------------------------------------%

% etiquetas_validas/2: el predicado etiquetas válidas verifica si en una lista
% todos sus elementos son menores o iguales a un cierto valor.

%% Caso base: es cierto que en una lista vacía todos sus elementos son menores
%% o iguales a cualquier valor (_).
etiquetas_validas([],_).

%% Caso recursivo: Se verifica que la cabeza de la lista sea menor o igual 
%% al límite dado (UpperLimit). Luego, se hace la llamada recursiva con la 
%% cola de la lista.
etiquetas_validas([H|T],UpperLimit) :-
	H =< UpperLimit,
	etiquetas_validas(T,UpperLimit).

%------------------------------------------------------------------------------%
%                    PREDICADOS AUXILIARES DE ESQUELETO 
%------------------------------------------------------------------------------%
%                     SUMAR LOS ELEMENTOS DE UNA LISTA               
%------------------------------------------------------------------------------%

% suma/2: suma los elementos de una lista, recibe la lista  

%% Caso base en el que se intenta sumas los elementos de una lista vacía. En
%% este caso se devuelve cero (0) como resultado.
suma([],0).

%% Caso recursivo para ir sumando los elementos de la lista [H|T]:
%%     - H : Cabeza de la lista a la cual se le quieren sumar los elementos.
%%     - T : Cola de la lista a la cual se le quieren sumar los elementos.
%%     - Sumalista : Almacena el resultado de la suma de los elementos.
%%     - R : Almacena el resultado parcial de la suma de los elementos.
suma([H|T],Sumalista) :-
	integer(H),
	suma(T,R),
	Sumalista is H + R.

%------------------------------------------------------------------------------%

%% sumarLista/2: predicado encargado de sumar todos los elementos de una lista
%% de listas.

%% Caso base en el que una lista de la lista no contenga elementos, su sumar
%% será cero (0)
sumarLista([[]],0).

%% Caso base en el que se intenten sumar los elementos de una lista vacía. 
%% De nuevo, el resultado será 0.
sumarLista([],0).

%% Caso recursivo para la suma de los las listas (elemento) de la lista. 
%% Para ello, se hará uso del predicado auxiliar suma/2.
%%     + H : Cabeza de la lista de listas.
%%     + T : Cola de la lista de listas
%%     - N : Suma de todos los elementos de la lista de listas.
%%     * N1 : Suma de los elementos de la lista H.
%%     * N2 : Suma parcial usada en la llamada recursiva del predicado.
sumarLista([H|T],N):-
	suma(H,N1),
	sumarLista(T,N2),
	N is N1+N2.

%------------------------------------------------------------------------------%
%                      GENERAR UNA LISTA DADO UN RANGO                         %
%------------------------------------------------------------------------------%

% generar/3: este predicado genera una lista con los elementos que se 
% encuentran en el rango [N,M] (inclusivo en ambos lados) 

%% Caso base de la recursión en el que la lista incluiría solo al 0 (valor
%% no válido).
generar(0,0,[]).

%% Caso base de la recursión en el que la lista correspondería a un sólo
%% elemento ya que N == M.
generar(N,M,[M]):- 	
	integer(N),
	integer(M),
	N==M.

%% Caso recursivo en el que se construye la lista de forma progresiva. La 
%% lista generada de devolverá en L, mientras que L1 corresponde a una 
%% construcción parcial de la misma.
generar(N,M,L):-
	integer(N),
	integer(M),
	N < M,
	N1 is N+1,
	generar(N1,M,L1),
	L = [N|L1].

%------------------------------------------------------------------------------%
%                         GENERADOR DE ESQUELETOS                              %
%------------------------------------------------------------------------------%

% generador/4: Dado el número de nodos N y un numero de nodos hijos "generados"
% genera una lista de enteros del largo de numero de hijos, suma los elementos 
%% de la misma y los resta a la cantidad de nodos.

%%
%% Caso base 1: Tanto el número de nodos como el numero de hijos es 0.
generador(N,NHijos,_,[]) :- integer(N), N==0, NHijos==0 ,!.

%% Caso base 2: El número de nodos es 0, pero el número de hijos es mayor a 0.
%% Entonces se terminará de completar la lista con 0's hasta que el 
%% número de hijos sea 0 también.
generador(N,NHijos,ListaG,L) :- 
	integer(N), 
	N==0,
	NHijos > 0,
	generar_esqueleto(N,NHijos,ListaG,_,_,L1),
	L = [L1],!.

%% Caso recursivo, en el que se va calculando el drop de la lista.
%%     + N : Número de nodos.	
%%     + NHijos : Largo de la lista que sera construida (representa el numero 
%%                de hujos de un nodo ).
%%     + ListaG : Es la lista de elementos que pueden pertenecer a la 
%%               lista que se generara.
%%     - L : Lista de todas las listas generadas por "generador" .
%%
generador(N,NHijos,ListaG,L) :-
	integer(N),
	integer(NHijos),
	generar_esqueleto(N,NHijos,ListaG,_,Suma,L1),
	Suma \= 0,
	X1 is N - Suma ,
	generador(X1,Suma,ListaG,L2),
	append([L1],L2,L).

%------------------------------------------------------------------------------%
%                  GENERADOR DE LAS LISTAS DEL ESQUELETO                       %
%------------------------------------------------------------------------------%

%% generar_esqueleto/5: Dado un número de nodos, una cantidad de hijos y una
%% lista de elementos "generar_esqueleto" genera una lista de la longitud del 
%% número de hijos con elementos que estén en la lista de elementos pasada.

%% Caso base 1: El número de hijos es 0, entonces la lista de retorno sera
%% vacía.
generar_esqueleto(_,NHijos,_,0,0,[]):- integer(NHijos), NHijos==0,!.

%% Caso base 2: La lista de elementos es vacía, entonces la lista de 
%% retorno sera vacía.
generar_esqueleto(_,_,[],0,0,[]).

%%  Caso base 3: El número de nodos es 0 pero los hijos son mayores que 0, 
%%				entonces la lista de elementos sera una lista de la longitud
%%				del número de hijos llenas de 0's.
generar_esqueleto(N,NHijos,ListaG,0,0,Esqueleto):- 
	integer(N),
	integer(NHijos), 
	N == 0,
	NHijos > 0,
	X1 is NHijos - 1,
	generar_esqueleto(N,X1,ListaG,_,_,L1),
	append([0],L1,Esqueleto).

%% Caso recursivo, Se van generando listas recursivamente.
%%     + N : Número de nodos.	
%%     + NHijos : Largo de la lista que sera construida (representa el numero 
%%                de hujos de un nodo ).
%%     + ListaG : Es la lista de elementos que pueden pertenecer a la 
%%               lista que se generara.
%%	   + Minimo  : El Minimo de los elementos de la lista
%%	   + Suma 	 : La suma de los elementos de la lista
%%     - Esqueleto : Lista de todas las listas generadas por "generador" .
%%
generar_esqueleto(N,NHijos,ListaG,Minimo,Suma,Esqueleto) :-
	integer(N),
	integer(NHijos),
	N > 0,
	NHijos > 0,
	member(X,ListaG),
	X =< N,
	X1 is N-X,
	X2 is NHijos - 1,
	generar_esqueleto(X1,X2,ListaG,Min,Suma2,L1), 
	X >= Min ,
	Minimo = X,
	Suma is Suma2 + X,
	append([X],L1,Esqueleto).

%------------------------------------------------------------------------------%
%                                 ESQUELETO                                    %
%------------------------------------------------------------------------------%

% esqueleto/3: Dado un número de nodos N y una aridad R el predicado 
%				"esqueleto" genera todos los posibles esqueletos.
%

%% Caso base: Si el número de nodos N es igual a 0 entonces el esqueletos
%%			  sera vacío.
%%
esqueleto(N,R,[]) :- integer(R), R > 0, N==0. 

%% Caso recursivo, Se van generando listas recursivamente.
%%     + N : Número de nodos.	
%%     + R : Aridad.
%%     - L : Esqueleto formado.
esqueleto(N,R,L) :-
	integer(N),
	integer(R),
	R>0,
	N> 0,
	generar(0,R,ListGen),
	member(X,ListGen),
	X \= 0,
	X =< N,
	XN is N-X-1,
	generador(XN,X,ListGen,LN),
	append([[X]],LN,L_aux),
	L = esq(L_aux).

%------------------------------------------------------------------------------%
%                                 ETIQUETABLE                                  %
%------------------------------------------------------------------------------%

% etiquetamiento/2: Dado un esqueleto "etiquetamiento" genera todos los
%					posibles arbole bien etiquetados a partir del mismo.
%

%% Caso base: Si el esqueleto esta vacío, entonces no se generara ningún 
%% árbol.
etiquetamiento(esq([]),[]).

%% Caso recursivo, Se van generando los arboles recursivamente.
%% Primero se calcula el número de nodos totales, se escoje una
%% etiqueta para la raíz del arbol y se genera una lista de elementos
%% para las aristas y para los nodos. Finalmente se generan todas las 
%% aristas de la raíz con "generar_aristas".
%%     + esq([H|T]) : Esqueleto dado.	
%%     - Arbol : Arbol generado.
etiquetamiento(esq([H|T]),Arbol) :- 
	suma(H,Result),
	sumarLista([H|T],N),
	N1 is N +1,
	generar(1,N1,Lista_nodo),
	delete(Lista_nodo,N1,Lista_arist),
	
	member(X,Lista_nodo),
	delete(Lista_nodo,X,Lista_nodo1),
	generar_aristas(Result,T,Lista_nodo1,Lista_arist,_,_,X,Arist),
	Arbol = nodo(X,Arist).

%------------------------------------------------------------------------------%

% generar_aristas/8: Dado un numero de nodos, un esqueleto, una lista de 
%%					 etiquetas para los nodos y otra para las aristas
%					 "generar_aristas" genera todas las aristas de dicho
%					 esqueleto y escoge etiquetas tanto para los nodos
%					 como para las aristas cumpliendo con las restricciones de 
%					 bien etiquetado.

%% Caso base1: Si el número de nodos es igual a 0 entonces la lista de aristas
%% es vacía.
generar_aristas(N,_,Lista_nodo,Lista_arist,LN,LA,_,[]):- 
	integer(N), 
	N==0,
	Lista_nodo = LN,
	Lista_arist = LA,!.

%% Caso base2: Si el esqueleto esta vacío, entonces la lista de aristas
%% es vacía.
generar_aristas(_,[],Lista_nodo,Lista_arist,LN,LA,_,[]):- 
	Lista_nodo = LN,
	Lista_arist = LA,!.

%% Caso base3: Si el esqueleto esta vacío, entonces la lista de aristas
%% es vacía.
generar_aristas(_,[[]],Lista_nodo,Lista_arist,LN,LA,_,[]):- 
	Lista_nodo = LN,
	Lista_arist = LA,!.
	
%% Caso recursivo:
%%     + N: Numero de nodos.
%%	   + Esquleto: Esqueleto a partir del cuál se generarán las aristas.
%%     + Lista_nodo: Lista de elementos disponibles para seleccionar
%%				     etiquetas de nodos.
%%     + Lista_arist: Lista de elementos disponibles para seleccionar
%%				      etiquetas de aristas.
%%     - LN: Lista de elementos restantes para seleccionar
%%				     etiquetas de nodos.
%%     - LA: Lista de elementos restantes para seleccionar
%%				     etiquetas de aristas.
%%	   + N_nod:	Etiqueta del nodo papa de la arista.
%%     - Arist : Aristas generadas.
generar_aristas(N,Esqueleto,Lista_nodo,Lista_arist,LN,LA,N_nod,Arist) :-
	integer(N),
	N > 0,
	N1 is N-1,
	[H|T] = Esqueleto,
	[H1|T1] = H,
	g_drop(H1,T,T2,Tail),
	append([Tail],T2,Nuevo_esqueleto2),
	append([T1],Nuevo_esqueleto2,Nuevo_esqueleto),

	member(X,Lista_nodo),
	member(Y,Lista_arist),
	Verificar is abs(N_nod-X),
	Y == Verificar,
	delete(Lista_nodo,X,Lista_nodo1),
	delete(Lista_arist,Y,Lista_arist1),

	generar_aristas(H1,T,Lista_nodo1,Lista_arist1,LN1,LA1,X,Arist1),
	generar_aristas(N1,Nuevo_esqueleto,LN1,LA1,LN2,LA2,N_nod,Arist2),
	LN = LN2,
	LA = LA2,
	append([arista(Y,nodo(X,Arist1))],Arist2,Arist).

%------------------------------------------------------------------------------%

% g_drop/4: Dado un numero N, y una lista g_drop utilia el predicado drop para
%			eliminar los N primeros elementos de la cabecera de una lista de 
%			listas.
%

%% Caso base: Si la lista es vacía entonces se devuleve una lista vacía
%%
g_drop(_,[],[],[]).

%% Caso recursivo:
%%     + N: Número de elementos a eliminar de una lista.
%%	   + Lista: Lista a la que se eliminaran elementos.
%%     - Tail :  Tail de "Lista"
%%     - Drop : Elementos de la cabecera de a lista a la cuál se le aplicó 
%%				"drop" 
g_drop(N,Lista,Tail,Drop):-
	[H|Tail] = Lista,
	drop(N,H,Drop),!.

%------------------------------------------------------------------------------%

% drop/3: Dado un número n, y una lista L, se obtiene la misma lista sin los 
% primeros n elementos.

%% En el caso de hacer drop a una lista vacía, se devuelve la misma.
drop(_,[],[]).

%% En el caso de que se quiera hacer drop de 0 elementos, se devuelve la misma
%% lista.
drop(N,Lista,Lista) :- integer(N), N == 0,!.

%% Caso recursivo, en el que se va calculando el drop de la lista.
%%     - N : Número de elementos que desean eliminarse de la lista.	
%%     - Lista : Lista a la cual se le eliminaran los elementos.
%%     - Tail : Lista a devolver, la cual tiene N elementos eliminados.
%%     - N1 : Número de elementos que desea eliminarse en la llamada recursiva.
%%     - T : Lista que será pasada a la llamada recursiva.
%%     - Tail2 : Valor de retorno de la llamada recursiva.
drop(N,Lista,Tail) :-
	integer(N),
	N1 is N-1,
	[_|T] = Lista,
	drop(N1,T,Tail2),!,
	Tail = Tail2.

%------------------------------------------------------------------------------%
%                            ESQ_ETIQUETABLE                                   %
%------------------------------------------------------------------------------%

% esqEtiquetable/2: predicado que verifica si todos los árboles R-arios con N
% nodos etiquetables son bien etiquetados

%% Caso en el que se quiere verificar el predicado con 0 nodos.
esqEtiquetable(0,R):- integer(R), R>=0.

%% Caso recursivo en el que se realiza la verificación de todas las posibles
%% opciones.
esqEtiquetable(N,R) :-
	integer(N),
	integer(R),
	forall((esqueleto(N,R,E),etiquetamiento(E,A)),bienEtiquetado(A)).

%------------------------------------------------------------------------------%
%                        DESCRIBIR ETIQUETAMIENTO                              %
%------------------------------------------------------------------------------%

%% describirEtiquetamiento/1: Se utiliza para agregarle a la llamada el nivel
%% de indentación inicial.
describirEtiquetamiento(Arbol) :- describirEtiquetamiento(0,Arbol).

% describirEtiquetamiento/2: Permite imprimir el árbol en un formato agradable
% para el usuario.

%% Caso recursivo para imprimir un nodo, y su lista de aristas asociada:
%%     - Indent : Corresponde al nivel de indentación asociado al predicado
%%		 actual.
%%     - X : Etiqueta asociada a la primera arista de la lista.
%%     - L : Lista de aristas asociadas al nodo relacionado con la primera lista.
describirEtiquetamiento(Indent,nodo(X,L)) :-
	nl, tab(Indent),
	write(nodo),write('('),write(X),
	NuevaIndent is Indent + 5,
	describirEtiquetamiento(NuevaIndent,L),
	write(')').

%% Caso base de la recursión para imprimir el caso en el que un nodo no tiene
%% aristas asociadas.
%%     - Indent : Corresponde al nivel de indentación asociado al predicado
%%		 actual.
describirEtiquetamiento(Indent,[]) :-
	nl, tab(Indent),
	write('[]').

%% Caso para imprimir una lista de aristas. Este utiliza el predicado Auxiliar
%% imprimirArista.
%%     - Indent : Corresponde al nivel de indentación asociado al predicado
%%		 actual.
%%     - X : Etiqueta asociada a la primera arista de la lista.
%%     - Y : Etiqueta asociada al nodo asociado a la primera arista.
%%     - L : Lista de aristas asociadas al nodo relacionado con la primera lista.
%%	   - T : Cola de la lista de aristas.
describirEtiquetamiento(Indent,[arista(X,nodo(Y,L))|T]) :-
	nl, tab(Indent),
	write('['),
	imprimirArista(Indent,[arista(X,nodo(Y,L))|T]),
	write(']').

%------------------------------------------------------------------------------%

% imprimirArista/2: Auxiliar de describirEtiquetamiento que permite imprimir
% la lista de aristas en un formato amigable para el usuario [1].

%% Caso base de la recursión para el caso en el que no hayan más aristas por
%% imprimir:
imprimirArista(_,[]).

%% Caso recursivo de la impresión para el caso en el que quedan más aristas por
%% imprimir.
%%     - Indent : Corresponde al nivel de indentación asociado al predicado
%%		 actual.
%%     - X : Etiqueta asociada a la primera arista de la lista.
%%     - Y : Etiqueta asociada al nodo asociado a la primera arista.
%%     - L : Lista de aristas asociadas al nodo relacionado con la primera lista.
%%	   - T : Cola de la lista de aristas.
imprimirArista(Indent,[arista(X,nodo(Y,L))|T]) :-
	write(arista), write('('), write(X),
	NuevaIndent is Indent + 8,
	describirEtiquetamiento(NuevaIndent,nodo(Y,L)),
	write(')'),
	nl,tab(Indent),
	imprimirArista(Indent,T).

%------------------------------------------------------------------------------%
%% Referencias:
%%
%%	   [1] - Ejemplo del formato de impresión para describirEtiquetamiento/2: 
%%	   - Entrada: describirEtiquetamiento(nodo(4,[arista(1,nodo(3,[])),
%%		arista(2,nodo(2,[])),arista(3,nodo(1,[]))])).         
%%	   - Impresión en pantalla:
%%				nodo(4
%%				 [arista(1
%%			         nodo(3
%%				              []))
%%				 arista(2
%%				         nodo(2
%%				              []))
%%				 arista(3
%%				         nodo(1
%%				              []))
%%				 ])
%%
%------------------------------------------------------------------------------%