%---------------------------------------------------------
% Estructuras
%---------------------------------------------------------

arista(X,A) :- 
	integer(X),
	X > 0,
	arbol(A).

arbol(nodo(X,[])) :- 
	integer(X),
	X > 0.
	
arbol(nodo(X,_)) :-
	integer(X),
	X > 0.

lista([],[],[]).
lista(arista(X,nodo(Y,A)),Ar,N):-
	lista(A,R1,R2),
	\+ member(X,R1),
	Ar = [X|R1],
	N =  [Y|R2].

lista([H|T],A,N) :-
	lista(H,A1,N1),
	lista(T,A2,N2),
	append(A1,A2,A),
	is_set(A),
	append(N1,N2,N),
	is_set(N).

%---------------------------------------------------------
% Predicado para verificar si un arbol esta bien etiquetado
%---------------------------------------------------------

bien_etiquetado(nodo(1,[])).
bien_etiquetado(nodo(X,[H|T])) :-
	bien_etiquetadoR(nodo(X,[H|T]),A,N),
	append(N,[X],NR),
	length(NR,LN),
	LA is LN - 1,
	etiquetas_validas(A,LA),
	etiquetas_validas(NR,LN).

bien_etiquetadoR(nodo(_,[]),_,_).
bien_etiquetadoR(nodo(X,[H|T]),A,N) :-
	buscarA(H,Arist,Nod),
	Etiqueta is abs(X-Nod),
	Etiqueta == Arist,
	lista([H|T],A,N),
	\+ member(X,N),
	bien_etiquetadoR(nodo(X,T),_,_).

buscarA(arista(X,nodo(Y,A)),X,Y) :-
	bien_etiquetadoR(nodo(Y,A),_,_).

etiquetas_validas([],_).
etiquetas_validas([H|T],UpperLimit) :-
	H =< UpperLimit,
	etiquetas_validas(T,UpperLimit).

%---------------------------------------------------------
% Predicados auxiliares para el esqueleto
%---------------------------------------------------------
% Predicado que suma los elementos de una lista 
%                (Se puede eliminar)
%---------------------------------------------------------

suma([],0).
suma([H|T],Sumalista) :-
	integer(H),
	suma(T,R),
	Sumalista is H + R.

sumarLista([[]],0).
sumarLista([],0).
sumarLista([H|T],N):-
	suma(H,N1),
	sumarLista(T,N2),
	N is N1+N2.
%---------------------------------------------------------
% Predicado que genera una lista dado un rango
%---------------------------------------------------------

generar(0,0,[]).
generar(N,M,[M]):- 	
	integer(N),
	integer(M),
	N==M.

generar(N,M,L):-
	integer(N),
	integer(M),
	N < M,
	N1 is N+1,
	generar(N1,M,L1),
	L = [N|L1].

%---------------------------------------------------------
%      Predicado generador de esqueletos
%---------------------------------------------------------

generador(N,NHijos,_,[]) :- integer(N), N==0, NHijos==0 ,!.
generador(N,NHijos,ListaG,L) :- 
	integer(N), 
	N==0,
	NHijos > 0,
	generar_esqueleto(N,NHijos,ListaG,_,Suma,L1),
	L = [L1],!.

generador(N,NHijos,ListaG,L) :-
	integer(N),
	integer(NHijos),
	generar_esqueleto(N,NHijos,ListaG,Min,Suma,L1),
	Suma \= 0,
	X1 is N - Suma ,
	generador(X1,Suma,ListaG,L2),
	append([L1],L2,L).

%---------------------------------------------------------
%    Predicado que genera la listas del esqueleto
%---------------------------------------------------------

generar_esqueleto(_,NHijos,_,0,0,[]):- integer(NHijos), NHijos==0,!.
generar_esqueleto(_,_,[],0,0,[]).
generar_esqueleto(N,NHijos,ListaG,0,0,Esqueleto):- 
	integer(N),
	integer(NHijos), 
	N == 0,
	NHijos > 0,
	X1 is NHijos - 1,
	generar_esqueleto(N,X1,ListaG,Min,Suma,L1),
	append([0],L1,Esqueleto).

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

%---------------------------------------------------------
%                Predicado esqueleto
%---------------------------------------------------------

esqueleto(N,R,[]) :- integer(R), R > 0, N==0. 
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

%---------------------------------------------------------
%                Predicado etiquetable
%---------------------------------------------------------

etiquetable([[]],[]).
etiquetable([H|T],Arbol) :- 
	suma(H,Result),
	sumarLista([H|T],N),
	N1 is N +1,
	generar(1,N1,Lista_nodo),
	delete(Lista_nodo,N1,Lista_arist),
	generar_aristas(Result,T,Lista_nodo,Lista_arist,LN,LA,N_nod,N_a,Arist),
	member(X,LN),
	Verificar is abs(N_nod - X),
	N_a == Verificar,
	Arbol = nodo(X,Arist).

generar_aristas(N,_,Lista_nodo,Lista_arist,LN,LA,N_nod,N_a,[]):- 
	integer(N), 
	N==0,
	member(N_nod,Lista_nodo),
	member(N_a,Lista_nodo),
	Lista_nodo = LN,
	Lista_arist = LA.

generar_aristas(N,Esqueleto,Lista_nodo,Lista_arist,LN,LA,N_nod,N_a,Arist) :-
	integer(N),
	N > 0,
	N1 is N-1,
	[H|T] = Esqueleto,
	[H1|T1] = H,
	append([T1],T,Nuevo_esqueleto),
	generar_aristas(H1,T,Lista_nodo,Lista_arist,LN1,LA1,N_nod1,N_a1,Arist1),

	member(X,LN1),
	Verificar is abs(N_nod1-X),
	N_a1 == Verificar,
	member(Y,LA1),
	delete(LN1,X,LN),
	delete(LA1,Y,LA),

	generar_aristas(N1,Nuevo_esqueleto,LN,LA,LN2,LA2,N_nod2,N_a2,Arist2),
	append([arista(Y,nodo(X,Arist1))],Arist2,Arist),
	N_nod = X,
	N_a = Y.


