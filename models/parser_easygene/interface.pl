:- ['../../lost.pl'].
:- lost_include_api(interface). 
:- lost_include_api(utils_parser_report).

% Generate a file of predicate based on Easygene report.


% Input Format Specification
lost_input_formats(annotate,[text(easygene_report)]).
% Output Format Specification
lost_output_format(annotate,_,[text(prolog(ranges(_)))]).


annotate([Easygene_Report],_Options,OutputFile) :-
	consult('eg_parser.pl'),  % Not nice 
	eg_parser(Easygene_Report,OutputFile).
