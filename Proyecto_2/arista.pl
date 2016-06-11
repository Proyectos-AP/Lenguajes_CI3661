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

bien_etiquetado(nodo(_,[])).
bien_etiquetado(nodo(X,[H|T])) :-
	buscarA(H,Arist,Nod),
	Etiqueta is abs(X-Nod),
	Etiqueta == Arist,
	lista([H|T],A,N),
	\+ member(X,N),
	bien_etiquetado(nodo(X,T)).

buscarA(arista(X,nodo(Y,A)),X,Y) :-
	bien_etiquetado(nodo(Y,A)).

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
	generar_esqueleto(N,NHijos,ListaG,L1),
	L = [L1],!.

generador(N,NHijos,ListaG,L) :-
	integer(N),
	integer(NHijos),
	generar_esqueleto(N,NHijos,ListaG,L1),
	suma(L1,Result),
	Result \= 0,
	X1 is N - Result,
	generador(X1,Result,ListaG,L2),
	append([L1],L2,L).

%---------------------------------------------------------
%    Predicado que genera la listas del esqueleto
%---------------------------------------------------------

generar_esqueleto(_,NHijos,_,[]):- integer(NHijos), NHijos==0,!.
generar_esqueleto(_,_,[],[]).
generar_esqueleto(N,NHijos,ListaG,Esqueleto):- 
	integer(N),
	integer(NHijos), 
	N == 0,
	NHijos > 0,
	X1 is NHijos - 1,
	generar_esqueleto(N,X1,ListaG,L1),
	append([0],L1,Esqueleto),!.

generar_esqueleto(N,NHijos,ListaG,Esqueleto) :-
	integer(N),
	integer(NHijos),
	N > 0,
	NHijos > 0,
	member(X,ListaG),
	X =< N,
	X1 is N-X,
	X2 is NHijos - 1,
	generar_esqueleto(X1,X2,ListaG,L1), 
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



