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
	Ar = R1,
	N =  [Y|R2].


lista([H|T],A,N) :-
	lista(H,A1,N1),
	lista(T,A2,N2),
	append(A1,A2,A),
	append(N1,N2,N).

bien_etiquetado(nodo(X,[])).
bien_etiquetado(nodo(X,[H|T])) :-
	buscarA(H,Arist,Nod),
	Etiqueta is abs(X-Nod),
	Etiqueta == Arist,
	lista([H|T],A,N),
	\+ member(Arist,A),
	\+ member(X,N),
	bien_etiquetado(nodo(X,T)).

buscarA(arista(X,nodo(Y,A)),X,Y) :-
	bien_etiquetado(nodo(Y,A)).


	
