:- ensure_loaded('../../shared/shared.pro').
:- ensure_loaded('lib.pro').


% Totaling salaries

getSalary(X,S) :- everything(add,mkQ(0,getSalary_),X,S).

getSalary_(salary(S),S).

add(X,Y,Z) :- Z is X + Y.


% Cutting salaries

cutSalary(X,Y) :- everywhere(mkT(cutSalary_),X,Y).

cutSalary_(salary(S1),salary(S2)) :- S2 is S1 / 2.


/*

% mkT-avoiding (disfavored) version 

cutSalary(X,Y) :- everywhere(cutSalary_,X,Y).
cutSalary_(X,Y) :-
  X = salary(S1) -> 
      ( S2 is S1 / 2, Y = salary(S2) )
    ; Y = X.

*/


:-
   msft(Company1),
   getSalary(Company1,S1),
   format('~w~n',[S1]),
   cutSalary(Company1,Company2),
   getSalary(Company2,S2),
   format('~w~n',[S2]),
   halt.
