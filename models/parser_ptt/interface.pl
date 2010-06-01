:- ['../../lost.pl'].
:- lost_include_api(interface). 
:- lost_include_api(utils_parser_report).

% Generate a file of predicate based on Easygene report.

lost_input_formats(annotate, [text(ptt)]).
lost_output_format(annotate, _Options, text(prolog(ranges(gene)))).

annotate([InputFile],_Options,OutputFile) :-
        consult(ptt_parser),
        gb_parser(InputFile,OutputFile).
