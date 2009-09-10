:- ensure_loaded('../../shared/shared.pro').
:- ensure_loaded('lib.pro').


% Firing expensive managers

fire(X,Y) :- everywhere(mkT(fire_),X,Y).

fire_(manager(_, salary(S)),_) :- S > 99999.


% Totaling salaries

getSalary(X,S) :- everything(add,mkQ(0,getSalary_),X,S).

getSalary_(salary(S),S).

add(X,Y,Z) :- Z is X + Y.


% Cutting salaries

cutSalary(X,Y) :- everywhere(mkT(cutSalary_),X,Y).

cutSalary_(salary(S1),salary(S2)) :- \+ var(S1), S2 is S1 / 2.


:-
   msft(Company1),
   getSalary(Company1,S1),
   format('~w~n',[S1]),
   fire(Company1,Company2),
   cutSalary(Company2,Company3),
   logvars(Company3,[Opening]),
   Opening = manager(name('Ralf'), salary(42)),
   getSalary(Company3,S2),
   format('~w~n',[S2]),
   halt.
