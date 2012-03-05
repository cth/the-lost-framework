infernal_results([[Left,Right,Score,EScore,PScore,GC]|Rest]) -->
	match(Left,Right,Score,EScore,PScore,GC),
	infernal_results(Rest).

infernal_results(Rest) -->
	garbage_line,
	infernal_results(Rest).

infernal_results([]) --> [].
	
match(Left,Right,Score,EScore,PScore,GC) -->
	match_position_line(Left,Right),
	match_score_line(Score,EScore,PScore,GC).

match_position_line(Left,Right) -->
	" Query = ", integer(_), " - ", integer(_),
	", ",
	"Target = ",integer(Left)," - ",integer(Right),
	"\n".

test_match_position_line :-
	match_position_line(1669950,1669851," Query = 1 - 100, Target = 1669950 - 1669851\n",[]).

match_score_line(Score,EScore,PScore,GC) -->
	" Score = ",
	score(Score),
%	{ write('matched score: '), writeln(Score) },
	", E = ",
	score(EScore),
%	{ write('matched escore: '), writeln(EScore) },
	", P = ",
	score(PScore),
%	{ write('matched pscore: '), writeln(PScore) },
	", GC =  ",
	score(GC),
%	{ write('matched GC: '), writeln(GC) },
	"\n".

test_match_score_line :-
	match_score_line(Score,EScore,PScore,GC," Score = 104.32, E = 9.433e-23, P = 1.224e-28, GC =  54\n",[]),
	writeln([Score,EScore,PScore,GC]).

score(scientific(Num)) -->
	scientific_number(Num).
score(float(Num)) -->
	float(Num).
score(integer(Num)) -->
	integer(Num).
		
garbage_line -->
	not_newlines,
	"\n".
	
not_newlines -->
	[X], 
	{ [X] \= "\n" },
	not_newlines.
not_newlines --> [].

scientific_number([Base,Exponent,AtomRepr]) -->
	% Base is a float
	float(Base),
	"e",
	sign(Sign),
	integer(Exponent1),
	{ 
		Exponent is Sign * Exponent1,
		term2atom(Base,AtomBase),
		term2atom(Exponent,AtomExp),
		atom_concat(AtomBase,e,Part1),
		atom_concat(Part1,AtomExp,AtomRepr)
	}.

sign(-1) --> "-".
sign(1) --> [].

integer(Integer) -->
	digits(Digits),
	{
		Digits \= [],
		atom_codes(Atom,Digits),
		atom_integer(Atom,Integer)
	}.
	
float(Float) -->
	integer(Part1),
	".",
	integer(Part2),
	{ move_after_comma(Part2,Part2Dec), Float is Part1 + Part2Dec }.
	
move_after_comma(X,Y) :-
    Y is X / 10,
    Y < 1.
move_after_comma(X,Y) :-
    Z is X / 10,
    Z >= 1,
    move_after_comma(Z,Y).

digits([]) --> [].	
digits([D|Ds]) -->
	digit(D),
	digits(Ds).

digit(D) --> [D], { atom_codes('0123456789',Digits), member(D,Digits) }.
	


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


test_parse_sci :-
	scientific_number([Base,Exp,AtomExpr],"1.224e-28",[]),
	writeln(AtomExpr),
	X is Base * (10 ** Exp),
	writeln(X).
	
	
	
test_parse_file :-
	readFile('infernal_search_107_1.gen',Codes),
	infernal_results(Results,Codes,[]),
	writeln(Results).
