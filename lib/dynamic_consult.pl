:- module(dynamic_consult, [dynamic_consult/1,dynamic_unconsult/1]).

/** <module> dynamic consulting and unconsulting of Prolog files 

This library allows you to consult a file, perform some queries on the contents of consulted file, and later
"unconsult" the file again.

@author: Christian Theil Have
*/

:- use_library(io).

% testing dynamic consulting

%% dynamic_consult(+File)
% Consult the prolog file File. All terms in the file will be asserted.
dynamic_consult(File) :-
	terms_from_file(File,Terms),
	assert(dyn_consult_terms(File,Terms)),
	forall(member(T,Terms),assert(T)).

dynamic_consult(File) :-
	dynamic_unconsult(File),
	fail.

%% dynamic_unconsult(+File)
% Undo consult of File. All asserted terms associated with File are retracted.
dynamic_unconsult(File) :-
	dyn_consult_terms(File,Terms),
	forall(member(T,Terms),retract(T)),
	retract(dyn_consult_terms(File,Terms)).
