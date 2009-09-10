:- ensure_loaded('../../shared/shared.pro').
:- ensure_loaded('lib.pro').

% Applicability condition

isSalary(salary(_)).


% Cutting salaries

cutSalary(salary(S1),salary(S2)) :- S2 is S1 / 2.
cutSalary(salary(S),salary(S)).


:-
   msft(Company1),

   % Determine number of options for salary cut; double default
   findall(
     Company2,
     everywhere(;(cutSalary,id),Company1,Company2),
     Companies1),
   length(Companies1,Len1),
   write(Len1),nl,

   % Determine number of options for salary cut
   findall(
     Company3,
     everywhere(mkT(isSalary,cutSalary),Company1,Company3),
     Companies2),
   length(Companies2,Len2),
   write(Len2),nl,

   halt.
