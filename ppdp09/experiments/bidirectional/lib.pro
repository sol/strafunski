:- ensure_loaded('../nonground/lib.pro').
:- abolish(everywhere/3).
:- abolish(gmapT/3).


% Transform everywhere

everywhere(T,X,Z) :-
  Apply = apply(T,[Y,Z]),
  Recurse = gmapT(everywhere(T),X,Y),
  ( var(X), \+ var(Z) ->
        Apply, Recurse
      ; Recurse, Apply ).


% Transform all immediate subterms

gmapT(T,X,Y) :-
    var(X), var(Y),
    Y = X
  ; 
    \+ var(X),
    X =.. [C|Kids1],
    map(T,Kids1,Kids2),
    Y =.. [C|Kids2]  
  ; 
    var(X), \+ var(Y),
    Y =.. [C|Kids2],
    map(T,Kids1,Kids2),
    X =.. [C|Kids1].
