:- module(terms, [term_has_rule_with_head/3,atom_integer/2,term_to_atom/2]).
/** <module> term manipulation

Utility predicates for term manipulation

@author: Christian Theil Have
*/

%% terms_has_rule_with_head(+Terms,+Functor,+Arity)
% True if the list Terms has a rule with a given Functor and Arity
terms_has_rule_with_head(Terms,Functor,Arity) :-
	member(Rule, Terms),
	Rule =.. [ (:-), Head, _ ],
	functor(Head, Functor, Arity).

%% atom_integer(?Atom,?Integer)
% Converts between atom representing an integer number to an 
% integer usuable in arithmetic operationes and vice versa.
atom_integer(Atom,Integer) :-
        ground(Atom),
        atom_chars(Atom, Chars),
        number_chars(Integer, Chars).

atom_integer(Atom,Integer) :-
        ground(Integer),
        number_chars(Integer,Chars),
        atom_chars(Atom,Chars).


%% term_to_atom(+Term,-Atom)
% Converts arbitrary Prolog terms to atoms.
term_to_atom(TermAtom,TermAtom) :-
	atom(TermAtom), !.
	
term_to_atom(Integer, Atom) :-
	integer(Integer), 
	!,
	atom_integer(Atom,Integer).
	
term_to_atom(Term,Atom) :-
	Term =.. [ Functor | Arguments ],
	atom(Functor),
	findall(ArgAtom,(member(Arg,Arguments), term_to_atom(Arg,ArgAtom)), ArgAtoms),
	intersperse(',',ArgAtoms,CommaSepArgAtoms),
	append([Functor, '('], CommaSepArgAtoms, Part1),
	append(Part1, [')'], Part2),
	atom_concat_list(Part2,Atom).