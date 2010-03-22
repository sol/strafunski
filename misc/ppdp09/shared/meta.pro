/*

 Metaprogramming convenience

*/


% Convert list into conjunction/disjunction and vv.

list_to_and([],true).
list_to_and([X],X).
list_to_and([X1,X2|Xs],(X1,X3)) :- list_to_and([X2|Xs],X3).

list_to_or([X],X).
list_to_or([X1,X2|Xs],(X1;X3)) :- list_to_or([X2|Xs],X3).

and_to_list(true,[]).
and_to_list((X1,X2),[X1|Xs]) :- and_to_list(X2,Xs).
and_to_list(X,[X]) :- \+ X = true, \+ X = (_,_).


% Find all clauses for a given predicate

clauses(Name/Arity,Clauses) :-
  findall(
    (Head:-Body),
    (functor(Head,Name,Arity), clause(Head,Body)),
    Clauses).
