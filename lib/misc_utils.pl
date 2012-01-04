%:- module(miscutils,[atom_concat_list]).
:- module(miscutils,[]).
/** <module> predicates that doesn't fit anywhere else 

Various useful utility predicates that doesn't fit anywhere else.

@author: Christian Theil Have

*/

:- lost_include_api(lists).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Conversion between upper case and lower case
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% upper_lower(?Upper,?Lower)
% Upper and Lower is a list of character codes. 
% Uppercase alphanumeric characters in Upper are converted to lowercase in Lower and vice versa.
upper_lower(Upper,Lower) :-
	ground(Upper),
	is_upper_case_alphanumeric(Upper),
	!,
	Lower is Upper + 32.

upper_lower(Upper,Lower) :-
	ground(Lower),
	is_lower_case_alphanumeric(Lower),
	!,
	Upper is Lower - 32.

% For everything non-alphanumeric
upper_lower(UpperLower,UpperLower) :-
	ground(UpperLower),
	not(is_upper_case_alphanumeric(UpperLower)),
	not(is_lower_case_alphanumeric(UpperLower)).

is_upper_case_alphanumeric(Code) :-
	Code >= 65,
	Code =< 90.

is_lower_case_alphanumeric(Code) :-
	Code >= 97,
	Code =< 122.