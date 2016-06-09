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

bien_etiquetado(nodo(X,[])).

bien_etiquetado(nodo(X,[H|T])) :-
	buscarA(H,Arist,Nod,Lista_nodos,Lista_aristas),
	Etiqueta is abs(X-Nod),
	Etiqueta == Arist,
	bien_etiquetado(nodo(X,T)).

buscarA(arista(X,nodo(Y,A)),X,Y,Lista_nodos,Lista_aristas) :-
	bien_etiquetado(nodo(Y,A)).


	
