%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Misc. small utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% List manipulation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% atom_concat_list(++List, --Atom)
% Concatenates all atoms in List in the order they appear
% to form a concatenation, Atom
atom_concat_list([Atom],Atom).
atom_concat_list([Elem1,Elem2|Rest], CompositeAtom) :-
	atom_concat(Elem1,Elem2,Elem3),
	atom_concat_list([Elem3|Rest], CompositeAtom).


inlists_nth0([], _, []).
inlists_nth0([List|RestLists], N, [Elem|RestElems]) :-
	nth0(N,List,Elem),
	inlists_nth0(RestLists,RestElems).

% Append variant which permit atom elements as first/second argument
flexible_append(A,B,[A,B]) :- atom(A), atom(B).
flexible_append(A,B,[A|B]) :- atom(A).
flexible_append(A,B,C) :- atom(B), append(A,[B],C).

% Merge list of lists into one long list, e.g.
% flatten_once([[a,b],[c,d],[e,f]],E) => E = [a, b, c, d, e, f].
flatten_once([],[]).
flatten_once([E1|Rest],Out) :-
	is_list(E1),
	append(E1,FlatRest,Out),	
	flatten_once(Rest,FlatRest).


map(F,InList,OutList) :-
	F =.. [ _ ],
	map_unary(F,InList,OutList).

map(F,InList,OutList) :-
	F =.. [ _ | ArgList ],
	ArgList \= [],
	map_with_arglist(F, InList, OutList).

% map applies to rule F(-,+) to each element of list
map_unary(_,[],[]).
map_unary(F, [L|Lists], [Out|OutRest]) :-
	Goal =.. [ F, L, Out], 
	call(Goal),
	map_unary(F,Lists,OutRest).


% Advanced map utility
map_with_arglist(_,[],[]).
map_with_arglist(F, [L|Lists], [Out|OutRest]) :-
	F =.. FList1,
	replace(input,L,FList1,FList2),
	replace(output,Out,FList2,FList3),
	NewF =.. FList3,
	call(NewF),
	map_with_arglist(F,Lists,OutRest).

list_head(List,Head) :- List = [ Head | _ ].
list_tail(List,Tail) :- List = [ _ | Tail ].

rotate_list_vector(ListOfEmptyLists,[]) :-
       forall(member(Elem, ListOfEmptyLists),Elem == []).

% e.g. rotate_list_vector([[1,1,1],[2,2,2],[3,3,3]],[[1,2,3],[1,2,3],[1,2,3]]).
rotate_list_vector(ListOfLists, [HeadsList|RotateTailsList]) :-
	map(list_head,ListOfLists,HeadsList),
	map(list_tail,ListOfLists,TailsList),
	rotate_list_vector(TailsList,RotateTailsList).


% replace(++Symbol,++Replacement,++Inlist,--Outlist)
% Outlist is a replicate of Inlist which has all instances
% of Symbol replaced with Replacement 
replace(_,_,[],[]).
replace(Symbol, Replacement, [Symbol|InListRest], [Replacement|OutListRest]) :-
	replace(Symbol,Replacement, InListRest,OutListRest).
replace(Symbol, Replacement, [Elem|InListRest], [Elem|OutListRest]) :-
	Symbol \= Elem,
	replace(Symbol,Replacement,InListRest,OutListRest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Term manipulation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% terms_has_rule_with_head(++Terms,++Functor,++Arity)
% True if the list Terms has a rule with a given Functor and Arity
terms_has_rule_with_head(Terms,Functor,Arity) :-
	member(Rule, Terms),
	Rule =.. [ (:-), Head, _ ],
	functor(Head, Functor, Arity).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Arithmetics
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Absolute value
abs(A,A) :-
	A >= 0.
abs(A,B) :-
	A < 0,
	B is A * -1.



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

% Find minimum of list
list_min([E],E).
list_min([E|R],Min) :-
	list_min(R,MR),
	((E < MR) -> Min = E ; Min = MR).


% atom_to_integer(++Atom,--Integer)
% Converts and atom representing an integer number to an
% Integer usuable in arithmetic operationes
atom2integer(Atom,Integer) :-
	atom_chars(Atom, Chars),
	number_chars(Integer, Chars).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Error checking
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% check_or_fail(Goal,Error):
% call Goal and throw and exception with error if Goal fails.
% Also, never backtrack beyond this point.
check_or_fail(Check,_Error) :-
	call(Check),
	!.

check_or_fail(_File,Error) :-
	throw(Error).

% check_or_warn(Goal,Error):
% call Goal and throw and exception with error if Goal fails.
% Also, never backtrack beyond this point.
check_or_warn(Check,_Error) :-
	call(Check),
	!.

check_or_warn(_File,Error) :-
        write('!!! '),
	writeq(warning(Error)),
        nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% File system
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Separate out the directory part of a filename
dirname(Filename,DirPartAtom) :-
	% everything before last '/'=47 is dirname:
	atom_codes(Filename, CharCodes),
	append(DirPart, FilePart, CharCodes),
	append(_,[47],DirPart), % DirPart should end with a '/'
	not(member(47,FilePart)), 
	atom_codes(DirPartAtom,DirPart).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ranges
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% overlaps(++Start1,++End1,++Start2,++End2)
% Determine if two ranges overlap
overlaps(Start1,End1, Start2,_) :-
        Start1 =< Start2,
        End1 >= Start2.
overlaps(Start1,_, Start2,End2) :-
        Start2 =< Start1,
        End2 >= Start1.

% Sums the number of positions in a list of ranges
sum_range_list([],0).
sum_range_list([[From,To]|Rest],Sum) :-
	LocalSum is To - From + 1,
	sum_range_list(Rest, RestSum),
	Sum is LocalSum + RestSum.
