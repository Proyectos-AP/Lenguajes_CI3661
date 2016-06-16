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
%%     - X : Etiqueta asociada al nodo.
%%     - A : Árbol asociado al nodo.
arista(X,A) :- 
	integer(X),
	X > 0,
	arbol(A).

%------------------------------------------------------------------------------%

%% arbol/2: Define la estructura de un nodo con las condiciones establecidas.
%% Es decir, posee una etiqueta (entero positivo) y una lista de aristas
%% asociada.

%% Caso en el que es una hoja (Lista vacía):  
%%     - X : Etiqueta asociada al nodo.
arbol(nodo(X,[])) :- 
	integer(X),
	X > 0.

%% Caso en el que el nodo tiene al menos un hijo en su lista de aristas.
%%     - X : Etiqueta asociada al nodo.
arbol(nodo(X,_)) :-
	integer(X),
	X > 0.

%------------------------------------------------------------------------------%

% lista/3: Dada la arista con todo su subarbol este predicado devuleve dos listas
%    una con todas las etiquetas de los nodos y otra con las etiquetas de las
%    aristas.

%% Caso base de la inicialización de las listas:
lista([],[],[]).

%% Caso en el que solo sea una arista
lista(arista(X,nodo(Y,A)),Ar,N):-
	lista(A,R1,R2),
	\+ member(X,R1),
	Ar = [X|R1],
	N =  [Y|R2].

%%
%% Caso en el que sea una lista de aristas
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

% bienEtiquetado/2: 
%

%% Caso base para verificar que un nodo esté bien etiquetado. En caso de que 
%% sea uno sólo, su etiqueta debe ser igual a uno (1).
bienEtiquetado(nodo(1,[])).

%% Caso cuando 
%%
bienEtiquetado(nodo(X,[H|T])) :-
	bienEtiquetadoR(nodo(X,[H|T]),A,N),
	append(N,[X],NR),
	length(NR,LN),
	LA is LN - 1,
	etiquetas_validas(A,LA),
	etiquetas_validas(NR,LN).

%------------------------------------------------------------------------------%

% bienEtiquetadoR/3:
%

%%
%%
bienEtiquetadoR(nodo(_,[]),_,_).

%%
%%
bienEtiquetadoR(nodo(X,[H|T]),A,N) :-
	buscarA(H,Arist,Nod),
	Etiqueta is abs(X-Nod),
	Etiqueta == Arist,
	lista([H|T],A,N),
	\+ member(X,N),
	bienEtiquetadoR(nodo(X,T),_,_).

%------------------------------------------------------------------------------%

% buscarA/3:
%

%%
%%
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

% sumarLista/2:
%

%%
%%
sumarLista([[]],0).

%%
%%
sumarLista([],0).

%%
%%
sumarLista([H|T],N):-
	suma(H,N1),
	sumarLista(T,N2),
	N is N1+N2.

%------------------------------------------------------------------------------%
%                      GENERAR UNA LISTA DADO UN RANGO                         %
%------------------------------------------------------------------------------%

% generar/3: Dado un rango N y M "generar" genera un lista de enteros de N a M,
%			 es decir, [N..M].

%% Caso en el que se genera una lista vacia.
%%
generar(0,0,[]).

%% Caso en el que se genera un singleton.
%%
generar(N,M,[M]):- 	
	integer(N),
	integer(M),
	N==M.


%% Caso recursivo, en donde se genera una lista de mas de un elemento.
%%     + N : Limite inferior de la lista a crear.	
%%     + M : Limite superior de la lista a crear.
%%     - L : Lista de elementos de N hasta M ([N..M]).
%%
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

% generador/4: Dado el numero de nodos N y un numero de nodos hijos "generados"
% 				genera una lista de enteros del largo de numero de hijos, suma 
%				los elementos de dicha lista y los resta a la cantidad de nodos.

%%
%% Caso base 1: Tanto el numero de nodos como el numero de hijos es 0.

generador(N,NHijos,_,[]) :- integer(N), N==0, NHijos==0 ,!.

%%
%% Caso base 2: El numero de nodos es 0 pero el numero de es mayor a 0.
%%              entonces se terminara de completar la lista con 0's hasta
%				que el numero de hijos sea 0 tambien.
%%

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

% generar_esqueleto/5: Dado un numero de nodos, una cantidad de hijos y una
%  						lista de elementos "generar_esqueleto" genera una lista
%						de la longitud del numero de hijos con elementos que esten
%						en la lista de elementos pasada.
%

%%  Caso base 1: El numero de hijos es 0, entonces la lista de retorno sera
%				 vacia.
%%
generar_esqueleto(_,NHijos,_,0,0,[]):- integer(NHijos), NHijos==0,!.

%%  Caso base 2: La lista de elementos es vacia, entonces la lista de 
%%				retorno sera vacia.
%%
generar_esqueleto(_,_,[],0,0,[]).

%%  Caso base 3: El numero de nodos es 0 pero los hijos son mayores que 0, 
%%				entonces la lista de elementos sera una lista de la longitud
%%				del numero de hijos llenas de 0's.
%%
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

% esqueleto/3: Dado un numero de nodos N y una aridad R el predicado 
%				"esqueleto" genera todos los posibles esqueletos
%

%% Caso base: Si el numero de nodos N es igual a 0 entonces el esqueletos
%%			  sera vacio.
%%
esqueleto(N,R,[]) :- integer(R), R > 0, N==0. 

%% Caso recursivo, Se van generando listas recursivamente.
%%     + N : Número de nodos.	
%%     + R : Aridad.
%%     - L : Esqueleto formado.
%%
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
	append([[X]],LN,L).

%------------------------------------------------------------------------------%
%                                 ETIQUETABLE                                  %
%------------------------------------------------------------------------------%

% etiquetamiento/2: Dado un esqueleto "etiquetamiento" genera todos los
%					posibles arbole =s bien etiquetados a partir del mismo.
%

%% Caso base: Si el esqueleto esta vacio, entonces no se generara ningun 
%%			  arbol.
%%
etiquetamiento([[]],[]).

%% Caso recursivo, Se van generando los arboles recursivamente.
%%		Primero se calcula el numero de nodos totales, se escoje una
%%		etiqueta para la raiz del arbol y se genera una lista de elementos
%%		para las aristas y para los nodos, finalmente se generan todas las 
%%		aristas de la raiz con "generar_aristas".
%%
%%     + esq([H|T]) : Esqueleto dado.	
%%     - Arbol : Arbol generado.
%%
etiquetamiento([H|T],Arbol) :- 
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

%% Caso base1: Si el numero de nodos es igual a 0 entonces la lista de aristas
%%			   es vacia.
%%
generar_aristas(N,_,Lista_nodo,Lista_arist,LN,LA,_,[]):- 
	integer(N), 
	N==0,
	Lista_nodo = LN,
	Lista_arist = LA,!.

%% Caso base2: Si el esqueleto esta vacio, entonces la lista de aristas
%%			   es vacia.
%%
generar_aristas(_,[],Lista_nodo,Lista_arist,LN,LA,_,[]):- 
	Lista_nodo = LN,
	Lista_arist = LA,!.

%% Caso base3: Si el esqueleto esta vacio, entonces la lista de aristas
%%			   es vacia.
%%
generar_aristas(_,[[]],Lista_nodo,Lista_arist,LN,LA,_,[]):- 
	Lista_nodo = LN,
	Lista_arist = LA,!.
	
%% Caso recursivo: .
%%
%%     + N: Numero de nodos.
%%	   + Esquleto: Esqueleto a partir del cual se generaran las aristas.
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
%%
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

%% Caso base: Si la lista es vacia entonces se devuleve una lista vacia
%%
g_drop(_,[],[],[]).

%% Caso recursivo: .
%%
%%     + N: Numero de elementos a eliminar de una lista.
%%	   + Lista: Lista a la que se eliminaran elementos.
%%     - Tail :  Tail de "Lista"
%%     - Drop : Elementos de la cabecera de a lista a la cual se le aplico 
%%				"drop" 
%%
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
%%     - Tail : 
%%     - N1 : Número de elementos que desea eliminarse en la llamada recursiva.
%%     - T : 
%%     - Tail2 :
drop(N,Lista,Tail) :-
	integer(N),
	N1 is N-1,
	[_|T] = Lista,
	drop(N1,T,Tail2),!,
	Tail = Tail2.

%------------------------------------------------------------------------------%
%                            ESQ_ETIQUETABLE                                   %
%------------------------------------------------------------------------------%

% esqEtiquetable/2:
%

%%
%%
esqEtiquetable(0,R):- integer(R), R>=0.

%%
%%
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

%% [1] - Ejemplo del formato de impresión para describirEtiquetamiento/2: 
%% - Entrada: describirEtiquetamiento(nodo(4,[arista(1,nodo(3,[])),
%%	arista(2,nodo(2,[])),arista(3,nodo(1,[]))])).         
%% - Impresión en pantalla:
%%			nodo(4
%%			 [arista(1
%%			         nodo(3
%%			              []))
%%			 arista(2
%%			         nodo(2
%%			              []))
%%			 arista(3
%%			         nodo(1
%%			              []))
%%			 ])

%------------------------------------------------------------------------------%