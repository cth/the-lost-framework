:- module(arithmetic, [abs/2, min/3, max/3, list_max/2, list_min/2]).
/** <module> arithmetic

Various arithmetic predicates.

@author: Christian Theil Have
*/

%% abs(+Number,-AbsoluteNumber) is det
% AbsoluteNumber is the absolute value of the integer Number.
abs(A,A) :-
	A >= 0.
abs(A,B) :-
	A < 0,
	B is A * -1.

%% min(+Number1,+Number2,Smallest) is det
% Smallest is the smallest of Number1 and Number2
min(A,A,A).
min(A,B,A) :- A < B.
min(A,B,B) :- B < A.

%% max(+Number1,+Number2,Largest) is det
% Largest is the largest of Number1 and Number2
max(A,A,A).
max(A,B,A) :- B < A.
max(A,B,B) :- A < B.

%% list_min(+List,-SmallestElement)
% SmallestElement is the smallest element in the list.
list_min([E],E).
list_min([E|R],Min) :-
	list_min(R,MR),
	((E < MR) -> Min = E ; Min = MR).
	
%% list_max(+List,-LargestElement)
% LargestElement is the largest element in the list.
list_max([E],E).
list_max([E|R],Max) :-
	list_max(R,MR),
	((E > MR) -> Max = E ; Max = MR).


