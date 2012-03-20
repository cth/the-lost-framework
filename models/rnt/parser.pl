:- ['../../lost.pl'].
:- use(misc_utils).
:- use(io).
% A DCG parser for gff result files

parse_rnt_file(GlimmerFile,PredictionsFile) :-
	readFile(GlimmerFile,Contents),!,
	rnt_parse(Predictions,Contents,[]),!,
	terms_to_file(PredictionsFile,Predictions).

rnt_parse(Genes) -->
	comment_line, % Organism line, e.g. "Methanosalsum zhilinae DSM 4017, complete genome. - 1..2138444"
	comment_line, % Number of RNAs line, e.g. "61 RNAs"
	comment_line, % Field description line, e.g. "Location	Strand	Length	PID	Gene	Synonym	Code	COG	Product"
	{!},
	genes(Genes).

genes([P|Ps]) -->
	gene(P),
	{write('.'),!},
	genes(Ps).

genes([]) --> [].

gene(gene(na,Left,Right,Strand,na,[pid(PID),synonym(Synonym),cog(COG),product(Product),length(Length)])) -->
	integer(Left),
	"..",
	integer(Right),
	"\t",
	strand(Strand),
	"\t",
	integer(Length),
	"\t",
	word(PID),
	"\t",
	word(Synonym),
	"\t",
	word(COG),
	"\t",
	word(_Unk1),
	"\t",
	word(_Unk2),
	"\t",
	match_characters_except([10,13],ProductCodes),
	{ atom_codes(Product,ProductCodes)},
	end_of_line.
	
word(Word) -->
	match_characters_except([9,32,10,13],WordCodes),
	{ atom_codes(Word,WordCodes) }.
	
strand('+') --> "+".
strand('-') --> "-".

comment_lines -->
	comment_line,
	comment_lines.
comment_lines --> [].

comment_line -->
	match_characters_except([10,13],_),
	end_of_line.
	
spaces --> space, spaces.
spaces --> [].

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

digits([D|Ds]) -->
	digit(D),
	digits(Ds).
digits([]) --> [].	

digit(D) --> [D], { atom_codes('0123456789',Digits), member(D,Digits) }.

test :-
	parse_rnt_file('CP002101.rnt','output.pl').
