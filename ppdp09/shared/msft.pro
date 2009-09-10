/*

 This is an appromximation of MSFT at some point in time.
 But again, do not take this seriously.

*/

msft(
   company([
     topdept(name('Human Resources'),
       manager(name('Lisa'), salary(123456)), []),
     topdept(name('Development'),
       manager(name('Anders'), salary(43210)), [
         subdept(name('Visual Basic'),
           manager(name('Amanda'), salary(8888)), []),
         subdept(name('Visual C#'),
           manager(name('Erik'), salary(4444)), [])])])).

