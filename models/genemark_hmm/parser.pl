:- ['../../lost.pl'].
:- lost_include_api(misc_utils).
:- lost_include_api(io).
% A DCG parser for prodigal result files

parse_genemark_hmm(ReportFile,PredictionsFile) :-
	readFile(ReportFile,Contents),
	genemark_hmm(Predictions,Contents,[]), 
	terms_to_file(PredictionsFile,Predictions).
	
genemark_hmm(Predictions) -->
	comment_lines,
	header_line1, 
	{!,write('hline1'),nl},	
	header_line2,
	{!,write('hline1'),nl},	
	header_line3,
	{!,write('hline3'),nl},		
	prediction_entries(Predictions),
	{write('here!'),nl}.

comment_lines --> [].
comment_lines --> 
	{writeln(comment_line)},
	comment_line,
	comment_lines.
	
comment_line --> 
	non_end_of_lines,
	end_of_line.

header_line1 -->
	"Predicted genes", 
	end_of_line.

header_line2 -->
	"   Gene    Strand    LeftEnd    RightEnd       Gene     Class",
	end_of_line.
	
header_line3 -->
	spaces, 
	"#",
	spaces,
	"Length",
	spaces,
	end_of_line.

	
prediction_entries([]) --> [].
prediction_entries([P|Ps]) --> 
	prediction_line(P),{!},
	prediction_entries(Ps).
	
prediction_line(prediction(na, Left, Right, Strand, Frame, [length(Length),class(Class),prediction_id(PredictionId)])) -->
	spaces,
	integer(PredictionId),
%	{writeln(prediction_id(PredictionId))},
	spaces,
	strand(Strand),
%	{writeln(strand(Strand))},
	spaces,
	maybe_overflow,
	integer(Left),
%	{!,writeln(left(Left))},
	spaces,
	maybe_overflow,
	integer(Right),
%	{writeln(right(Right))},	
	spaces,
	integer(Length),
%	{writeln(length(Length))},
	spaces,
	integer(Class),
%	{writeln(class(Class))},
	end_of_line,
	{
		% Need to cross-check that calculation frame corresponds
		% to what genbank / refseq says
		Temp is Left mod 3,
		(Temp = 0 ->
            Frame = 3
        ;
            Frame = Temp
        )
	}.
	
strand('+') --> "+".
strand('-') --> "-".

maybe_overflow -->  [].
maybe_overflow -->  "<".
maybe_overflow -->  ">".

non_end_of_lines --> [].
non_end_of_lines --> 
	non_end_of_line,
	non_end_of_lines.
	
non_end_of_line -->
	[ X ],
	{ not(member(X,[10,13])) }.
	
end_of_line --> end_of_line(_).
end_of_line(windows)--> [10,13].                        % windows end of line
end_of_line(unix) --> [10].    % unix end of line

space --> [ 9 ]. % tab character
space --> [ 32 ]. % normal space character

spaces --> [].
spaces --> space, spaces.

integer(Integer) -->
	digits(Digits),
	{
		Digits \= [],
		atom_codes(Atom,Digits),
		atom_integer(Atom,Integer)
	}.
	
digits([D|Ds]) -->
	digit(D),
	digits(Ds).
digits([]) --> [].

digit(D) --> [D], { atom_codes('0123456789',Digits), member(D,Digits) }.




%%%%%%%%%
% Tests

test_comment1 :-
	Comment = "GeneMark.hmm PROKARYOTIC (Version 2.6r)",
	append(Comment,[10],Line),
	comment_line(Line,[]).

test_prediction1 :-
	Prediction = "    1        +          <3          98           96        1",
	append(Prediction,[10],Line),
	prediction_line(P,Line,[]),
	writeln(P).