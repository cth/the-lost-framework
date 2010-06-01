:- ['../../lost.pl'].
:- lost_include_api(interface). 
:- lost_include_api(utils_parser_report).

% Generate data predicates form fna file of Genbank

% Option declaration
lost_option(annotate,list,'none','Divided sequences of Nucleotids into pieces').

% Input Format Specification
lost_input_formats(annotate,[text(fna),text(gbk)]).
% Output Format Specification
lost_output_format(annotate,_,text(prolog(sequence(dna)))).


annotate([FNA_File,GBK_File],Options,OutputFile) :-
        consult(fna_parser),
        parser_fna(Options,FNA_File,GBK_File,OutputFile).
