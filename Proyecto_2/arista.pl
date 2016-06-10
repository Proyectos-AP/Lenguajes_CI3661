arista(X,A) :- 
	integer(X),
	X > 0,
	arbol(A).

arbol(nodo(X,[])) :- 
	integer(X),
	X > 0.
	
arbol(nodo(X,[H|T])) :-
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

bien_etiquetado(nodo(X,[])).
bien_etiquetado(nodo(X,[H|T])) :-
	buscarA(H,Arist,Nod),
	Etiqueta is abs(X-Nod),
	Etiqueta == Arist,
	lista([H|T],A,N),
	\+ member(X,N),
	bien_etiquetado(nodo(X,T)).

buscarA(arista(X,nodo(Y,A)),X,Y) :-
	bien_etiquetado(nodo(Y,A)).


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

esqueleto(N,R,[]) :- integer(R), R > 0,N==0. 
esqueleto(N,R,L) :-
	integer(N),
	generar(1,N,ListGen),
	member(X,ListGen),
	X1 is N-X,
	esqueleto(X1,R,L1),
	append([[X]],L1,L).

