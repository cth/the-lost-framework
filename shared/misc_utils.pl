%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Misc. small utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Parse an lost model interface file to a set of rules
terms_from_file(File, Terms) :-
	open(File, read, Stream),
	ground(Stream),
	collect_stream_terms(Stream,Terms),
	close(Stream).

terms_to_file(File,Terms) :-
	open(File,write,Stream),
	ground(Stream),
	write_terms_to_stream(Stream,Terms),
	close(Stream).

write_terms_to_stream(_,[]).
write_terms_to_stream(Stream,[Term|Rest]) :-
	writeq(Stream,Term),
	write(Stream,'.\n'),
	write_terms_to_stream(Stream,Rest).


% Create list of Rules found in Stream
collect_stream_terms(Stream, Rules) :-
	read(Stream, T),
	((T == end_of_file) ->
		Rules = []
	;
		collect_stream_terms(Stream,Rest),
		append([T],Rest,Rules)
	).

terms_has_rule_with_head(Terms,Functor,Arity) :-
	member(Rule, Terms),
	Rule =.. [ (:-), Head, _ ],
	functor(Head, Functor, Arity).


% atom_concat_list(+List, -Concatenation)
atom_concat_list([Atom],Atom).
atom_concat_list([Elem1,Elem2|Rest], CompositeAtom) :-
	atom_concat(Elem1,Elem2,Elem3),
	atom_concat_list([Elem3|Rest], CompositeAtom).


max(A,B,A) :- A > B.
max(A,B,B) :- A =< B.

atom2integer(Atom,Integer) :-
	atom_chars(Atom, Chars),
	number_chars(Integer, Chars).


% check_or_fail(Goal,Error):
% call Goal and throw and exception with error if Goal fails.
% Also, never backtrack beyond this point.
check_or_fail(Check,_Error) :-
	call(Check),
	!.

check_or_fail(_File,Error) :-
	throw(Error).


% Separate out the directory part of a filename
dirname(Filename,DirPartAtom) :-
	% everything before last '/'=47 is dirname:
	atom_codes(Filename, CharCodes),
	append(DirPart, FilePart, CharCodes),
	append(_,[47],DirPart), % DirPart should end with a '/'
	not(member(47,FilePart)), 
	atom_codes(DirPartAtom,DirPart).



