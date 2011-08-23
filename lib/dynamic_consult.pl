:- lost_include_api(io).

% testing dynamic consulting

dynamic_consult(File) :-
	terms_from_file(File,Terms),
	assert(dyn_consult_terms(File,Terms)),
	forall(member(T,Terms),assert(T)).

dynamic_consult(File) :-
	dynamic_unconsult(File),
	fail.

dynamic_unconsult(File) :-
	dyn_consult_terms(File,Terms),
	forall(member(T,Terms),retract(T)),
	retract(dyn_consult_terms(File,Terms)).