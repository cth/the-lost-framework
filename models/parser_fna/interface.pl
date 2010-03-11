:- ['../../lost.pl'].
:- lost_include_api(interface). 
:- lost_include_api(utils_parser_report).

% Generate a file of predicate based on Easygene report.

lost_best_annotation([FNA_File,GBK_File],Options,OutputFile) :-
        consult(fna_parser),
        parser_fna(Options,FNA_File,GBK_File,OutputFile).
