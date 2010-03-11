:- ['../../lost.pl'].
:- lost_include_api(interface). 
:- lost_include_api(utils_parser_report).

% Generate a file of predicate based on Easygene report.

lost_best_annotation([InputFile],_Options,OutputFile) :-
        consult(gm_parser),
        gm_parser(InputFile,OutputFile).
