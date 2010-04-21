:- ['../../lost.pl'].
:- lost_include_api(interface). 
:- lost_include_api(utils_parser_report).

% Generate data predicates form fna file of Genbank

% Option declaration
lost_option(lost_best_annotation,list,280,'Divided sequences of Nucleotids into pieces').

% Input Format Specification
lost_input_formats(lost_best_annotation,[text(fna),text(gbk)]).
% Output Format Specification
lost_output_format(lost_best_annotation,_,[text(prolog(sequence(dna)))]).


lost_best_annotation([FNA_File,GBK_File],Options,OutputFile) :-
        consult(fna_parser),
        parser_fna(Options,FNA_File,GBK_File,OutputFile).
