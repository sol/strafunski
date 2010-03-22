/*

 Higher-order logic programming snippets.

*/

% Left-biased choice for first-order functions

choice(F,G,X,Y) :- 
  apply(F,[X,Y]) ->
      true
    ; apply(G,[X,Y]).


% Void choice

choice(F,_,X) :- apply(F,[X]), !.
choice(_,G,X) :- apply(G,[X]).


% Symmetric choice for first-order functions

';'(F,G,X,Y) :- apply(F,[X,Y]); apply(G,[X,Y]).


% Function composition

compose(F,G,X,Z) :- apply(F,[X,Y]), apply(G,[Y,Z]).


% Constant functions

const(R,_,R).


% Flip arguments of a binary function or predicate

flip(P,X,Y) :- apply(P,[Y,X]).

flip(F,X,Y,Z) :- apply(F,[Y,X,Z]).


% Guarding a function application by a predicate

guard(P,F,X,Y) :- apply(P,[X]), apply(F,[X,Y]).


% Identity function

id(X,X).


% Void identity

id(_).


% Extend incomplete function applications by passing more arguments

pass(T1,Xs,T2) :-
  T1 =.. [C|Ts1],
  append(Ts1,Xs,Ts2),
  T2 =.. [C|Ts2].

