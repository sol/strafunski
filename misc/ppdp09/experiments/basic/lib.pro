% Transform everywhere

everywhere(T,X,Z) :-
  gmapT(everywhere(T),X,Y),
  apply(T,[Y,Z]).


% Query (and crush) everything

everything(F,Q,X,Z) :-
  gmapQ(everything(F,Q),X,Y),
  apply(Q,[X,R]),
  foldl(F,R,Y,Z).


% Transform all immediate subterms

gmapT(T,X,Y) :-
  X =.. [C|Kids1],
  map(T,Kids1,Kids2),
  Y =.. [C|Kids2].


% Query all immediate subterms

gmapQ(Q,X,Y) :-
  X =.. [_|Kids],
  map(Q,Kids,Y).
