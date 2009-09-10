/*

 Some shared operations not depending on the experiment in question.
 Most of the SYB operations though are to be found in the lib.pro files. 

*/


% Make transformation

mkT(T,X,Y) :- choice(T,id,X,Y).


/*

% Less combinatorial choice

mkT(T,X,Y) :- apply(T,[X,Y]) -> true; Y = X.

*/


% "make transformation" with explicit applicability condition

mkT(AC,T,X,Y) :- apply(AC,[X]) -> apply(T,[X,Y]); Y = X.


% Void variation

mkT(T,X) :- choice(T,id,X).


% Make query

mkQ(R,Q,X,Y) :- apply(Q,[X,Y]) -> true; Y = R.


% "make query" with explicit applicability condition

mkQ(AC,R,Q,X,Y) :- apply(AC,[X]) -> apply(Q,[X,Y]); Y = R.
