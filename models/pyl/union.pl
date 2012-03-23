% Simple union-find implementation in Prolog
:- dynamic set/2.

% Create set, if it is not allready created
makeset(X) :-
	(set(X,_) ->
		true
		;
		assert(set(X,X))).
	
find(Root,Root) :-
	set(Root,Root).

find(Element,Root) :-
	ground(Element),
	set(Element,Parent),
	Element \= Parent,
	find(Parent,Root).

find2(Root,Root) :-
	set(Root,Root).

find2(Elem,Root) :-
	ground(Root),
	set(Elem,Root),
	Elem \= Root.

find2(Elem,Root) :-
	ground(Root),
	set(Child,Root),
	Child \= Root,
	find2(Elem,Child).
	
roots(Roots) :-
	findall(Root,find(Root,Root),Roots).

transitive_closure(Root,Elements) :-
	findall(Element,find2(Element,Root),Elements).

union(X,X).

union(X,Y) :-
	find(X,RootX),
	find(Y,RootY),
	retract(set(RootX,_XRootParent)),
	assert(set(RootX,RootY)).
	
union_find_test :-
	Set = [ 1,2,3,4,5,6 ],
	forall(member(X,Set),makeset(X)),
	union(1,3),
	union(2,4),
	union(2,6).
	/*
	find(2,Root2),
	transitive_closure(Root2,Xs),
	sort(Xs,[2,4,6]). */