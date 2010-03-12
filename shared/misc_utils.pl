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


%max(A,B,A) :- A > B.
%max(A,B,B) :- A =< B.

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


% map applies to rule F(-,+) to each element of list
map(_,[],[]).
map(F, [L|Lists], [Out|OutRest]) :-
	Goal =.. [ F, L, Out], 
	call(Goal),
	map(F,Lists,OutRest).


% Advanced map utility
multi_map(_,[],[]).
multi_map(F, [L|Lists], [Out|OutRest]) :-
	F =.. FList1,
	replace(input,L,FList1,FList2),
	replace(output,Out,FList2,FList3),
	NewF =.. FList3,
	call(NewF),
	multi_map(F,Lists,OutRest).


replace(_,_,[],[]).
replace(Symbol, Replacement, [Symbol|InListRest], [Replacement|OutListRest]) :-
	replace(Symbol,Replacement, InListRest,OutListRest).
replace(Symbol, Replacement, [Elem|InListRest], [Elem|OutListRest]) :-
	Symbol \= Elem,
	replace(Symbol,Replacement,InListRest,OutListRest).


rm_seq_elems([],[]).
rm_seq_elems([[AnnotType,From,To,_]|Rest1],[[AnnotType,From,To]|Rest2]) :-
	rm_seq_elems(Rest1,Rest2).

inlists_nth0([], _, []).
inlists_nth0([List|RestLists], N, [Elem|RestElems]) :-
	nth0(N,List,Elem),
	inlists_nth0(RestLists,RestElems).
		
% Sums the number of positions in a list of ranges
sum_range_list([],0).
sum_range_list([[From,To]|Rest],Sum) :-
	LocalSum is To - From + 1,
	sum_range_list(Rest, RestSum),
	Sum is LocalSum + RestSum.

% Find minimum element
min(A,A,A).
min(A,B,A) :- A < B.
min(A,B,B) :- B < A.

% Find maximum of two elements
max(A,A,A).
max(A,B,A) :- B < A.
max(A,B,B) :- A < B.

% Find maximum of list
list_max([E],E).
list_max([E|R],Max) :-
	list_max(R,MR),
	((E > MR) -> Max = E ; Max = MR).

% Append variant which permit atom elements as first/second argument
flexible_append(A,B,[A,B]) :-
	atom(A),atom(B).
flexible_append(A,B,[A|B]) :-
	atom(A).
flexible_append(A,B,C) :-
	atom(B),
	append(A,[B],C).

% Merge list of lists into one long list, e.g.
% flatten_once([[a,b],[c,d],[e,f]],E) => E = [a, b, c, d, e, f].
flatten_once([],[]).
flatten_once([E1|Rest],Out) :-
	is_list(E1),
	append(E1,FlatRest,Out),	
	flatten_once(Rest,FlatRest).

% Determine if two ranges overlap
overlaps(Start1,End1, Start2,_) :-
        Start1 =< Start2,
        End1 >= Start2.
overlaps(Start1,_, Start2,End2) :-
        Start2 =< Start1,
        End2 >= Start1.

