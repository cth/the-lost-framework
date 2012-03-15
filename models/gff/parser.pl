:- ['../../lost.pl'].
:- use(misc_utils).
:- use(io).
% A DCG parser for gff result files

parse_gff_file(GlimmerFile,PredictionsFile) :-
	readFile(GlimmerFile,Contents),
	gff_parse(Predictions,Contents,[]), 
	terms_to_file(PredictionsFile,Predictions).

gff_parse(Genes) -->
	comment_lines,
	{!},
	genes(Genes),
	comment_lines.

genes([P|Ps]) -->
	gene(P),
	{write('.'),!},
	genes(Ps).

genes([]) --> [].

gene(gene(Organism,Left,Right,Strand,Frame,[gene_type(Type),source(Source),score(Score)])) -->
	organism_def(Organism),
	spaces,
	source(Source),
	spaces,
	gene_type(Type),
	spaces,
	integer(Left),
	spaces,
	integer(Right),
	spaces,
	score(Score),
	spaces,
	strand(Strand),
	spaces,
	frame(Frame),
	attributes(_Attributes),
	end_of_line.
	
organism_def(Org) --> word(Org).

source(Source) --> word(Source).
		
gene_type(Type) --> word(Type).

attributes(X) -->
	match_characters_except([10,13],X).
	
word(Word) -->
	match_characters_except([9,32,10,13],WordCodes),
	{ atom_codes(Word,WordCodes) }.
	
score(na) --> ".".
score(Score) --> float(Score).
	
strand('+') --> "+".
strand('-') --> "-".

frame(na) --> "." ; "0".
frame(1) --> "1".
frame(2) --> "2".
frame(3) --> "3".


comment_lines -->
	comment_line,
	comment_lines.
comment_lines --> [].

comment_line -->
	"#",
	match_characters_except([10,13],_),
	end_of_line.
	
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
	
test :-
	parse_gff_file('CP002028.gff','output.pl').
