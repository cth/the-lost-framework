:- ['../../lost.pl'].
:- lost_include_api(interface). 
:- lost_include_api(utils_parser_report).

% Input Format Specification
lost_input_formats(annotate,[text(report_genemark)]).
% Output Format Specification
lost_output_format(annotate,_,text(prolog(ranges(_)))).


% Generate a file of predication based on Genemark  report.
annotate[InputFile],_Options,OutputFile) :-
        consult(gm_parser),
        gm_parser(InputFile,OutputFile).
