:- module('lists ',[atom_concat_list/2,inlists_nth0/3,flexible_append/3,flatten_once/2,map/3,map_with_arglist/3,rotate_list_vector/2,replace/4,match_tail/3,not_member/2,intersperse/3,take/3,split_list/4,zip/3]).

/** <module> working with lists. 

List manipulation predicates.

@author: Christian Theil Have

*/

:- lost_include_api(misc_utils).
:- use(terms).

%% atom_concat_list(+List, -Atom) is det
% Concatenates all atoms in List in the order they appear
% to form a concatenation, Atom
atom_concat_list([Atom],Atom).
atom_concat_list([Elem1,Elem2|Rest], CompositeAtom) :-
	term_to_atom(Elem1,Elem1Atom),
	term_to_atom(Elem2,Elem2Atom),
	atom_concat(Elem1Atom,Elem2Atom,Elem3),
	atom_concat_list([Elem3|Rest], CompositeAtom).

% Assumes each atom to be exactly one character
atom_list_code_list([],[]).
atom_list_code_list([Atom|AtomsRest],[Code|CodesRest]) :-
	atom_codes(Atom, [Code]),
	atom_list_code_list(AtomsRest,CodesRest).

%% inlists_nth0(+ListOfLists,+N,-NthElemFromEachList).
% Extract the n'th element (0 based indexing) from a list of lists
inlists_nth0([], _, []).
inlists_nth0([List|RestLists], N, [Elem|RestElems]) :-
	nth0(N,List,Elem),
	inlists_nth0(RestLists,N,RestElems).

%% flexible_append(+L1,+L2,-L3) is det
% Append variant which permit atom elements as first/second argument
flexible_append(A,B,[A,B]) :- atom(A), atom(B).
flexible_append(A,B,[A|B]) :- atom(A).
flexible_append(A,B,C) :- atom(B), append(A,[B],C).

%% flatten_once(+ListIn,-ListOut) is det
% Merge list of lists into one long list, e.g.
% ==
% flatten_once([[a,b],[c,d],[e,f]],E) => E = [a, b, c, d, e, f].
% ==
% Note that unlike the more generic flatten predicate, this only flattens out 
% one nesting level
flatten_once([],[]).
flatten_once([[]|Rest],OutRest) :-
        !,
        flatten_once(Rest,OutRest). 
flatten_once([A|Rest],[A|OutRest]) :-
	atom(A),
        !,
	flatten_once(Rest,OutRest).
flatten_once([E1|Rest],Out) :-
	is_list(E1),
	append(E1,FlatRest,Out),
        !,
	flatten_once(Rest,FlatRest).


%% map(+Goal,+InList,+OutList)
% applies the deterministic Goal (which must be arity two) to each element of InList producing OutList
% E.g. if A is member of InList, then for Goal =.. [ Functor, A, B], call Goal and B is a member of OutList
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

%% map_with_arglist(+GoalPattern,+InList,-OutList) is det
% More flexible version of map//3. 
% GoalPattern is a special Prolog goal with an argument list, where one of the arguments can be =|input|= and 
% one of the arguments is named =|output|=. The goal will be called for each element in InList as replacing 
% each argument named =|input|= and =|output|= being a variable corresponding an element from OutList.
% As example consider,
% == 
% map_with_arglist(atom_concat(input,input,output),[a,b],[aa,bb]).
% ==
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


%% replace(+Symbol,+Replacement,+Inlist,-Outlist)
% Outlist is a replicate of Inlist which has all instances
% of Symbol replaced with Replacement 
replace(_,_,[],[]).
replace(Symbol, Replacement, [Symbol|InListRest], [Replacement|OutListRest]) :-
	atom(Symbol),
	replace(Symbol,Replacement, InListRest,OutListRest).
replace(Symbol, Replacement, [Elem|InListRest], [Elem|OutListRest]) :-
	atom(Symbol),
	Symbol \= Elem,
	replace(Symbol,Replacement,InListRest,OutListRest).

%% replace_list(+MatchList,?ReplacementList,+InList,?OutList)
% OutList is InList with every occurence of sublist MatchList replaced by ReplacementList
replace_list(_,_,[],[]).
	
replace_list(MatchList,ReplaceList,InList,OutList) :-
	append(MatchList,Suffix,InList),!,
	replace_list(MatchList,ReplaceList,Suffix,OutListRest),
	append(ReplaceList,OutListRest,OutList).
	
replace_list(MatchList,ReplaceList,[Elem|InList],[Elem|OutList]) :-
	replace_list(MatchList,ReplaceList,InList,OutList).
	

%% filter(+filter_pred,+InList,-OutList)
% OutList is the subset of InList for which filter_pred is true
filter(_,[],[]).
	
filter(FilterPred,[X|Xs],[X|Ys]) :-
	Predicate = [ FilterPred, X ],
	call(Predicate),
	!,
	filter(FilterPred,Xs,Ys).
	
filter(FilterPred,[X|Xs],Ys) :-
	filter(FilterPred,Xs,Ys).

%% match_tail(+InputList,-HeadOfInputList,+TailOfInputList)
% true if InputList ends with TailOfInputList
match_tail(Match,[],Match).
match_tail([H|T],[H|Hr],Match) :- match_tail(T,Hr,Match).

%% not_member(+Elt,+List)
% true is Elt is not a member of List
% Note: well-behaved for Elt and List ground
not_member(Elt,List) :-
        member(Elt,List),
        !,
        false.

not_member(_Elt,_List).

%% intersperse(?Separator,UnseparatedList,SeparatedList)
% Intersperse a list with a particular separator
% e.g. 
% ==
% intersperse(',', ['a','b','c'], ['a',',','b,',','c'])
% ==
intersperse(_,[],[]).
intersperse(_,[One],[One]).
intersperse(Separator,[One,Two|Rest],[One,Separator|NewRest]) :-
        intersperse(Separator,[Two|Rest],NewRest).

%% take(+N,+ListIn,-ListOut). 
% true if ListOut is the first N elements of ListIn
take(0, _, []).
take(N, [E|R1],[E|R2]) :-
        N1 is N - 1,
        !,
        take(N1,R1,R2).


%% split_list(+N,+List,-FirstPart,-LastPart) is det
% FirstPart is the first N elements of List
% LastPart is the remaining
split_list(_, [], [], []).
split_list(N, [E|List], [E|ListHead], ListTail) :-
        N > 0,
        N1 is N - 1,
        !,
        split_list(N1,List,ListHead,ListTail).
split_list(N, [E|List], [], [E|ListTail]) :-
        N =< 0,
        N1 is N - 1,
        !,
        split_list(N1,List,[],ListTail).

%% zip(+List1,+List2,ZippedList)
% Combines two lists into one
zip([],_,[]).
zip(_,[],[]).
zip([E1|L1],[E2|L2],[[E1,E2]|ZipRest]) :-
        zip(L1,L2,ZipRest).
