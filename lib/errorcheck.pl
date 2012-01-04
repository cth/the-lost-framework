:- module(errorcheck,[check_or_fail/2, check_or_warn/2]).

/** <module> convenient handling of errors/exceptions.

This library contains predicates to easy exception handling. 

author: Christian Theil Have
*/

%% check_or_fail(Goal,Error)
% call Goal and throw and exception with error if Goal fails.
% Also, never backtrack beyond this point.
check_or_fail(Check,_Error) :-
	call(Check),
	!.

check_or_fail(_File,Error) :-
	throw(Error).

%% check_or_warn(Goal,Error)
% call Goal and warn with error if Goal fails.
% Also, never backtrack beyond this point.
check_or_warn(Check,_Error) :-
	call(Check),
	!.

check_or_warn(_File,Error) :-
        write('!!! '),
	writeq(warning(Error)),
        nl.

