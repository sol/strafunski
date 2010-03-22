:- ensure_loaded('../../shared/shared.pro').
:- ensure_loaded('lib.pro').


% Totaling salaries

getSalary(X,S) :- everything(add,mkQ(0,getSalary_),X,S).

getSalary_(salary(S),S).


% Increasing salaries

incSalary(X,Y) :- everywhere(mkT(incSalary_),X,Y).

incSalary_(salary(S1),salary(S2)) :- add(S1,1,S2).


% Multi-mode addition

add(X,Y,Z) :-
 ( \+ var(X), \+ var(Y), Z is X + Y
 ; \+ var(X), \+ var(Z), Y is Z - X
 ; \+ var(Y), \+ var(Z), X is Z - Y 
 ).


:-
   msft(Company1),
   getSalary(Company1,S1),
   format('~w~n',[S1]),
   incSalary(Company2,Company1),
   getSalary(Company2,S2),
   format('~w~n',[S2]),
   halt.
