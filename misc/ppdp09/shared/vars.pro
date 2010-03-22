/*

 Operations dealing with lists/sets of logical/nonground variables.

*/


% Membership test for lists of logic variables

varmember(E,[H|T]) :-
  var(E),
  var(H), 
  ( E == H; varmember(E,T) ).


% Left-biased union of two sets of logic variables

varunion(X,[],X).
varunion(X0,[H|T],Y) :-
  ( varmember(H,X0) -> 
      varunion(X0,T,Y)
    ; ( append(X0,[H],X1),
        varunion(X1,T,Y) )).


% Test a list to constitute a set of logic variables

varset([]).
varset([H|T]) :- var(H), \+ varmember(H,T), varset(T).
