:- ['../../lost.pl'].
:- lost_include_api(misc_utils).
:- lost_include_api(io).
% A DCG parser for prodigal result files

parse_glimmer_file(GlimmerFile,PredictionsFile) :-
	readFile(GlimmerFile,Contents),
	glimmer_parse(Predictions,Contents,[]), 
	terms_to_file(PredictionsFile,Predictions).

glimmer_parse(Predictions) -->
	definition_line(_),
	predictions(Predictions).

predictions([P|Ps]) -->
	prediction(P),
	predictions(Ps).

predictions([]) --> [].

prediction(glimmer(na,Left,Right,Strand,Frame,[score(Score)])) -->
	"orf",
	integer(Id),
	spaces,
	integer(Start),
	spaces ,
	integer(End),
	spaces,
	strand(Strand),
	integer(Frame),
	spaces,
	float(Score),
	end_of_line,
	{
		((Start < End) ->
			Left = Start,
			Right = End
			;
			Left = End,
			Right = Start
		)
	}.
	
strand('+') --> "+".
strand('-') --> "-".
	
definition_line(Def) -->
	">",
	match_characters_except([10,13],Def),
	end_of_line.

spaces --> [].
spaces --> space, spaces.

match_characters_except(Exceptions) -->
	match_characters_except(Exceptions,_).

match_characters_except(_Exceptions,[]) --> [].
match_characters_except(Exceptions,[C|Cs]) -->
	match_character_except(Exceptions,C),
	match_characters_except(Exceptions,Cs).

match_character_except(Exceptions,C) -->
	[ C ],
	{ not(member(C, Exceptions)) }.
	
non_end_of_lines([Code|Rest]) --> non_end_of_line(Code), non_end_of_lines(Rest).
non_end_of_lines([Code]) --> non_end_of_line(Code).

non_end_of_line(Code) --> [ Code ], { not(member(Code, [10,13])) }.

end_of_line --> end_of_line(_).
end_of_line(windows)--> [10,13].                        % windows end of line
end_of_line(unix) --> [10].    % unix end of line

space --> [ 9 ]. % tab character
space --> [ 32 ]. % normal space character

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

test1 :-
	D = ">gi|49175990|ref|NC_000913.2| Escherichia coli str. K-12 substr. MG1655 chromosome, complete genome",
	append(D,[10],L),
	definition_line(Def,L,[]),
	writeln(Def).
