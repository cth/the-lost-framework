:- ['../../lost.pl'].
:- lost_include_api(interface). 
:- lost_include_api(utils_parser_report).

% Generate a file of predicate based on Easygene report.


% Input Format Specification
lost_input_formats(lost_best_annotation,[text(easygene_report)]).
% Output Format Specification
lost_output_format(lost_best_annotation,_,[text(prolog(ranges(_)))]).


lost_best_annotation([Easygene_Report],_Options,OutputFile) :-
	consult('eg_parser.pl'),  % Not nice 
	eg_parser(Easygene_Report,OutputFile).
