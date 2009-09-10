/*

 Metaprogramming support for generating traversal code

*/


% Generate a transformation

completeT(Name,Sort) :-
  clauses(Name/2,ClausesO),
  abolish(Name/2),
  skippableSorts(ClausesO,Sort,Skip),
  generateT(Skip,Name,Sort,ClausesG),
  override(ClausesG,ClausesO,Clauses),
  map(assert,Clauses),
  compile_predicates([Name/2]).


% Determine types that are reached only trivially by traversal

skippableSorts(Clauses,Sort,Skip) :-
  byCases(Clauses),
  reachable_sorts(Sort,Reachable),
  mapunique(inferSort(Reachable),Clauses,Overridden),
  filter(skippableSort(Overridden),Reachable,Skip).

skippableSort(Overridden,Candidate) :-
  reachable_sorts(Candidate,Reachable),
  intersection(Reachable,Overridden,[]).


% Infer type cover from input patterns

inferSort(Reachable,Clause,Sort) :-
  clause_to_case(Clause,Term1),
  functor(Term1,C,Arity),
  findall(S,
    (
      member(S,Reachable),
      sort_to_clauses(S,Clauses),
      member((Head:-_),Clauses),
      Head =.. [_,Term2],
      functor(Term2,C,Arity) 
    ),
    [Sort]).


% Generate a query

completeQ(F,Z,Name,Sort) :-
  clauses(Name/2,ClausesO),
  abolish(Name/2),
  skippableSorts(ClausesO,Sort,Skip),
  generateQ(Skip,F,Z,Name,Sort,ClausesG),
  override(ClausesG,ClausesO,Clauses),
  map(assert,Clauses),
  compile_predicates([Name/2]).


% Override operation assuming case discrimination based on term patterns

override(Clauses1,Clauses2,Clauses3) :-
  byCases(Clauses1,Pred),
  byCases(Clauses2,Pred),
  mapaccum(override_,Clauses2,Clauses1,Clauses3).

override_(Clause1,Clauses1,Clauses2) :-
  clause_to_case(Clause1,Term1),
  append(Clauses1a,[Clause2|Clauses1b],Clauses1),
  clause_to_case(Clause2,Term2),
  Term1 =@= Term2,
  append(Clauses1a,[Clause1|Clauses1b],Clauses2).


% Test for a predicate to be defined by case discrimination

byCases(Clauses) :-
  byCases(Clauses,_).

byCases(Clauses,Pred) :-
  clauses_to_pred(Clauses,Pred),
  mapunique(clause_to_case,Clauses,Terms),
  length(Clauses,Len),
  length(Terms,Len).
  

% Extract predicate symbol from clauses

clauses_to_pred(Clauses,Pred) :-
  mapunique(clause_to_pred,Clauses,[Pred]).

clause_to_pred(Clause,Name/Arity) :-
  (
  Clause = (Head:-_) -> 
  true ;
  Clause = Head ),
  functor(Head,Name,Arity).


% Extract input patterns from clauses

clause_to_case(Clause,Term) :-
  (
  Clause = (Head:-_) -> 
  true ;
  Clause = Head ),
  Head =.. [_,Term|_].


% Generate a transformation

generateT(Name,Sort,Cs) :-
  findall(X,simple(X),Skip),
  generateT(Skip,Name,Sort,Cs).

generateT(Skip,Name,Sort,Cs) :-
  reachable_sorts(Sort,Sorts),
  map(generateT_(Skip,Name),Sorts,Css),
  concat(Css,Cs).

generateT_(Skip,Name,Sort,Cs0) :-
  member(Sort,Skip) -> 
      Cs0 = []
    ; sort_to_clauses(Sort,Cs1),
      map(generateT__(Skip,Name),Cs1,Cs0).

generateT__(Skip,Name,Clause,(Head:-Body)) :-
  sort_clause(Clause,_Sort,C,Vars1,Lits),
  Head =.. [Name,X,Y],
  map(generateT___(Skip,Name),Vars1,Lits,Vars2,Gss),
  concat(Gss,Gs),
  list_to_and(Gs,Body),
  X =.. [C|Vars1],
  Y =.. [C|Vars2].

generateT___(Skip,Name,Var1,Lit,Var2,Gs) :-
    Lit =.. [Sort,Var1] ->
      ( member(Sort,Skip) -> Var2 = Var1, Gs = []
      ; G =.. [Name,Var1,Var2], Gs = [G] 
      ) 
  ; 
    Lit = map(Sort,Var1),
    ( member(Sort,Skip) -> Var2 = Var1, Gs = [] 
    ; G = map(Name,Var1,Var2), Gs = [G]
    ).


% Generate a query

generateQ(F,Z,Name,Sort,Cs) :-
  findall(X,simple(X),Skip),
  generateQ(Skip,F,Z,Name,Sort,Cs).

generateQ(Skip,F,Z,Name,Sort,Cs) :-
  reachable_sorts(Sort,Sorts),
  map(generateQ_(Skip,F,Z,Name),Sorts,Css),
  concat(Css,Cs).

generateQ_(Skip,F,Z,Name,Sort,Cs0) :-
  member(Sort,Skip) -> 
      Cs0 = []
    ; sort_to_clauses(Sort,Cs1),
      map(generateQ__(Skip,F,Z,Name),Cs1,Cs0).

generateQ__(Skip,F,Z,Name,Clause,(Head:-Body)) :-
  sort_clause(Clause,_Sort,C,Vars1,Lits),
  Head =.. [Name,X,Y],
  X =.. [C|Vars1],
  map(generateQ___(Skip,F,Z,Name),Vars1,Lits,Varss2,Gss),
  concat(Varss2,Vars2),
  concat(Gss,Gs1),
  ( Vars2 = [Var1] ->
        Gs2 = [], Y = Var1
      ; ( Vars2 = [Var1,Var2] -> 
              pass(F,[Var1,Var2,Y],G),
              Gs2 = [G]
            ; Gs2 = [foldl(F,Z,Vars2,Y)] )),
  append(Gs1,Gs2,Gs3),
  list_to_and(Gs3,Body).

generateQ___(Skip,F,Z,Name,Var1,Lit,Vars2,Gs) :-
  Lit =.. [Sort,Var1] -> ( 
  member(Sort,Skip) ->
  Vars2 = [], 
  Gs = [] ; 
  Vars2 = [Var2],
  G =.. [Name,Var1,Var2],
  Gs = [G] ) ; 
  Lit = map(Sort,Var1),
  member(Sort,Skip) ->
  Vars2 = [], 
  Gs = [] ; 
  Vars2 = [Var2],
  Gs = [mapreduce(Name,F,Z,Var1,Var2)].
