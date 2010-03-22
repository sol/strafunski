:- ensure_loaded('../../shared/shared.pro').
:- ensure_loaded('lib.pro').


% Types for the company example

company(company(L))     :- map(topdept,L).
topdept(topdept(N,M,L)) :- alias(dept(N,M,L)).
manager(manager(N,S))   :- alias(person(N,S)).
subunit(subdept(N,M,L)) :- alias(dept(N,M,L)).
subunit(employee(N,S))  :- alias(person(N,S)).
salary(salary(N))       :- number(N).
name(name(A))           :- atom(A).


% Type aliases used above

dept(N,M,L) :- name(N), manager(M), map(subunit,L).
person(N,S) :- name(N), salary(S).


% Totaling salaries

getSalary(salary(S),S).

add(X,Y,Z) :- Z is X + Y.

:- completeQ(add,0,getSalary,company).


% Cut salaries

cutSalary(salary(S1),salary(S2)) :- S2 is S1 / 2.

:- completeT(cutSalary,company).


:-
   msft(Company1),
   getSalary(Company1,S1),
   format('~w~n',[S1]),
   cutSalary(Company1,Company2),
   getSalary(Company2,S2),
   format('~w~n',[S2]),
   halt.
