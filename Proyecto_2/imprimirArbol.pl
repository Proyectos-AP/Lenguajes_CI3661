%------------------------------------------------------------------------------%
%                            DESCRIBIR ETIQUETAMIENTO                          %
%------------------------------------------------------------------------------%

describirEtiquetamiento(Arbol) :- describirEtiquetamiento(0,Arbol).

describirEtiquetamiento(Indent,nodo(X,L)) :-
	nl, tab(Indent),
	write(nodo),write('('),write(X),
	NuevaIndent is Indent + 5,
	describirEtiquetamiento(NuevaIndent,L),
	write(')').

describirEtiquetamiento(Indent,[]) :-
	nl, tab(Indent),
	write('[]').


describirEtiquetamiento(Indent,[arista(X,nodo(Y,L))|T]) :-
	nl, tab(Indent),
	write('['),
	imprimirArista(Indent,[arista(X,nodo(Y,L))|T]),
	write(']').

imprimirArista(_,[]).

imprimirArista(Indent,[arista(X,nodo(Y,L))|T]) :-
	write(arista), write('('), write(X),
	NuevaIndent is Indent + 8,
	describirEtiquetamiento(NuevaIndent,nodo(Y,L)),
	write(')'),
	nl,tab(Indent),
	imprimirArista(Indent,T).

% nodo(4,[arista(1,nodo(3,[])),arista(2,nodo(2,[])),arista(3,nodo(1,[]))])