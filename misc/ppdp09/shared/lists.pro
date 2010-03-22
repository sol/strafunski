/* 

 (Mainly higher-order) list processing

*/


% Map for first-order functions

map(_,[],[]).
map(F,[X|Xs],[Y|Ys]) :- apply(F,[X,Y]), map(F,Xs,Ys).


% Void map

map(_,[]).
map(P,[X|Xs]) :- apply(P,[X]), map(P,Xs).


% Further arities

map(_,[],[],[]).
map(F,[X1|X1s],[X2|X2s],[X3|X3s]) :-
  apply(F,[X1,X2,X3]),
  map(F,X1s,X2s,X3s).

map(_,[],[],[],[]).
map(F,[X1|X1s],[X2|X2s],[X3|X3s],[X4|X4s]) :-
  apply(F,[X1,X2,X3,X4]),
  map(F,X1s,X2s,X3s,X4s).


% Map with removing doubles

mapunique(F,Xs,Zs) :-
  map(F,Xs,Ys),
  list_to_set(Ys,Zs).


% Map with accumulator

mapaccum(_,[],A,A).
mapaccum(F,[X|Xs],A1,A3) :- apply(F,[X,A1,A2]), mapaccum(F,Xs,A2,A3).


% Map and reduce

mapreduce(F,G,Z,X,Y) :- foldl(mapreduce_(F,G),Z,X,Y).
mapreduce_(F,G,X1,X2,Y) :- apply(F,[X2,X3]), apply(G,[X1,X3,Y]).


% Filter a list according to a predicate

filter(_,[],[]).
filter(P,[X|Xs],Ys1) :- 
  filter(P,Xs,Ys2),
  ( apply(P,[X]) -> Ys1 = [X|Ys2] ; Ys1 = Ys2 ). 


% Left-associative fold

foldl(_,Z,[],Z).
foldl(F,Z,[X|Xs],Y0) :- apply(F,[Z,X,Y1]), foldl(F,Y1,Xs,Y0).


% Right-associative fold

foldr(_,Z,[],Z).
foldr(F,Z,[X|Xs],Y0) :- foldr(F,Z,Xs,Y1), apply(F,[X,Y1,Y0]).


% An illiustrative application of foldr

concat(Xss,Xs) :- foldr(append,[],Xss,Xs).


% Zipping two lists

zip([],[],[]).
zip([X|Xs],[Y|Ys],[(X,Y)|XYs]) :- zip(Xs,Ys,XYs).


% Unzipping a list

unzip([],[],[]).
unzip([(X,Y)|XYs],[X|Xs],[Y|Ys]) :- unzip(XYs,Xs,Ys).


% Construct a singleton list

singleton(X,[X]).
