/*

Subset of definite clause programs that may serve as type definitions.
What you see here is really just an illustration.

*/


% This is a marker predicate for type aliases.

alias(G) :- G.


% Simple types

simple(atom).
simple(number).


% Look up clauses for a sort

sort_to_clauses(Sort,Clauses) :-
  simple(Sort) ->
      Clauses = [] 
    ; clauses(Sort/1,Clauses). 


% Compute sort closure for a given seed sort

reachable_sorts(S,Ss) :- reachable_sorts(S,[],Ss).

reachable_sorts(S,Ss1,Ss2) :-
  member(S,Ss1) ->
      Ss2 = Ss1
    ; (
        sort_to_sorts(S,Ss3),
        mapaccum(reachable_sorts,Ss3,[S|Ss1],Ss2)
      ).  


% Determine sorts immediately referenced by its definition

sort_to_sorts(S,Ss) :-
  sort_to_clauses(S,Cs),
  map(clause_to_sorts,Cs,Sss),
  concat(Sss,Ss).

clause_to_sorts(Clause,Ss) :-  
  sort_clause(Clause,_,_,_,Lits),
  map(lit_to_sort,Lits,Ss).

lit_to_sort(map(S,_),S).
lit_to_sort(Lit,S) :- Lit =.. [S,_].


% Check clauses for sorts

sort_clause(Head :- Body) :-
  sort_clause((Head :- Body),_,_,_,_).

sort_clause((Head :- Body1),S,C,Vars,Lits) :-
  sort_head(Head,S,C,Vars),
  varset(Vars),
  unalias_body(Body1,Body2),
  sort_body(Body2,Vars,Lits).


% Check heads of clauses for sorts

sort_head(Head,S,C,Args) :-
  Head =.. [S,Arg],
  Arg =.. [C|Args],
  map(var,Args).


% Macro expansion for aliases

unalias_body(alias(Head),Body) :-
  predicate_property(Head,number_of_clauses(1)),
  clause(Head,Body).
unalias_body(Body,Body) :-
  \+ Body = alias(_).


% Check bodies of clauses for sorts

sort_body((Lit,Body),[Var|Vars],[Lit|Lits]) :-
  sort_lit(Lit,Var),
  sort_body(Body,Vars,Lits).
sort_body(Lit,[Var],[Lit]) :-
  sort_lit(Lit,Var).


% Check literals in bodies of clauses for sorts

sort_lit(Lit,Var) :-
  Lit =.. [F|Args],
  \+ F = (,),
  append(Test,[Arg],Args),
  var(Arg),
  Arg == Var,
  ground(Test).
