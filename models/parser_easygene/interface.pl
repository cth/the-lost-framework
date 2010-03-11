:- ['../../lost.pl'].
:- lost_include_api(interface). 
:- lost_include_api(utils_parser_report).

% Generate a file of predicate based on Easygene report.

test(A,B) :-
        parser_line(A,B).

lost_best_annotation([Easygene_Report],_Options,OutputFile) :-
	consult('eg_parser.pl'),  % Not nice 
	eg_parser(Easygene_Report,OutputFile).
