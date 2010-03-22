:- 
   ensure_loaded('../basic/lib.pro'),
   abolish(gmapT/3),
   abolish(gmapQ/3).


% Transform all immediate subterms

gmapT(T,X,Y) :-
  var(X) -> Y = X
  ; ( X =.. [C|Kids1],
      map(T,Kids1,Kids2),
      Y =.. [C|Kids2] ).


% Query all immediate subterms

gmapQ(Q,X,Y) :-
  var(X) -> Y = []
  ; ( X =.. [_|Kids],
      map(Q,Kids,Y) ).


% Collect all variables in a term

logvars(X,Y) :- everything(varunion,mkQ([],logvars_),X,Y).
logvars_(X,[X]) :- var(X).
