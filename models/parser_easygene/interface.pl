:- ['../../lost.pl'].
:- lost_include_api(interface). 
:- lost_include_api(utils_parser_report).

% Generate a file of predicate based on Easygene report.

lost_best_annotation([Easygene_Report],Options,OutputFile) :-
        lost_restricted_option(Options,output_name,OutputName),
        lost_sequence_from_file(OutputName,OutputFile),
	eg_parser(Easygene_Report,OutputFile).
