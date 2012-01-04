% Fast orf chopper

% Find all stops
% e.g. 
% pos frame strand
% stop(1231,'+',3)

triplet(Pos,'+',[]) :-
	base(Pos,Base1,Pos1),
	base(Pos1,Base2,Pos2),
	base(Pos2,Base3,_).

triplet(Pos,'-',[Rev3,Rev2,Rev1]) :-
	rev_base(Pos,Base1,Pos),
	rev_base(Pos1,Base2,Pos2),
	rev_base(Pos2,Base3,_),
	compl(Base1,Rev1),
	compl(Base2,Rev2),
	compl(Base3,Rev3).

compl(a,t).
compl(t,a).
compl(a,g).
compl(g,a).


