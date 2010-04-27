% Parses .ldata format produced by genemark when running
% with the -D option 

:- ['../../lost.pl'].
:- lost_include_api(misc_utils).
:- lost_include_api(utils_parser_report).

ldata_parser_main(InputFile,OutputFile) :-
	open(InputFile,read,InputStream2),
	open(OutputFile,write,OStream),
	set_output(OStream),
        (ldata_parser(InputStream2) ; true),
	close(OStream),
	close(InputStream2).

ldata_parser(IS) :-
        readline(IS,NextLine),
        ldata_parser_rec(IS,NextLine).

% End of file:
ldata_parser_rec(_,[-1]) :- 
        !.

% Empty lines:
ldata_parser_rec(IS,[]) :-
        !,
        ldata_parser(IS).

% Comment lines
ldata_parser_rec(IS,[35|Rest]) :-
        !,
        atom_codes(Comment_Atom,[37,35|Rest]),
        write(Comment_Atom),nl,
        ldata_parser(IS).

ldata_parser_rec(IS, Line) :-
	!,
        parser_line(Line,LineTokens),
        ldata_line(Term,LineTokens,[]),
        writeq(Term), write('.'), nl,
        ldata_parser(IS).


% DCG for parsing differeggxnt type of lines
ldata_line(T) --> gene_prediction_line(T).
ldata_line(T) --> region_prediction_line(T).
ldata_line(T) --> frame_shift_prediction_line(T).

gene_prediction_line(genemark_gene_prediction(Start,End,Strand,Frame,[average_probability(AvgProb),start_codon_probability(StartProb)])) -->
        [Start,End],
        reading_frame6(Strand,Frame),
        [AvgProb,StartProb],
	{ integer(Start),integer(End), float(AvgProb),float(StartProb) }.

region_prediction_line(genemark_region_prediction(Start,End,Strand,Frame)) -->
        [Start,End],
        reading_frame6(Strand,Frame),
	{ integer(Start),integer(End) }.

frame_shift_prediction_line(genemark_frame_shift_prediction(FromFrame,ToFrame,Position,Strand)) -->
        [FromFrame,ToFrame,Position],
        strand_word(Strand),
	{ integer(ToFrame),integer(FromFrame) }.

reading_frame6(+,Frame) -->
        [ Frame ],
	{ member(Frame, [1,2,3]) }.

reading_frame6(-,Frame) -->
        [ Frame1 ],
        { member(Frame1,[4,5,6]), Frame is Frame1 - 3 }.

strand_word(+) --> [ direct ]. 
strand_word(-) --> [ complement ].
