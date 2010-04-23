:- ['../../lost.pl'].
:- lost_include_api(interface). 
:- lost_include_api(utils_parser_report).

% Input Format Specification
lost_input_formats(lost_best_annotation,[text(report_genemark)]).
% Output Format Specification
lost_output_format(lost_best_annotation,_,text(prolog(ranges(_)))).


% Generate a file of predication based on Genemark  report.
lost_best_annotation([InputFile],_Options,OutputFile) :-
        consult(gm_parser),
        gm_parser(InputFile,OutputFile).
