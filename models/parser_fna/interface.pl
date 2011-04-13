:- ['../../lost.pl'].
:- lost_include_api(interface). 
:- lost_include_api(utils_parser_report).

% Generate data predicates form fna file of Genbank

% Option declaration
lost_option(annotate,list,'none','Divided sequences of Nucleotids into pieces').
lost_option(annotate,range,[min,max],'Define a range to extract a sub-part of the genome').
lost_option(parse,list,'none','Divided sequences of Nucleotids into pieces').
lost_option(parse,range,[min,max],'Define a range to extract a sub-part of the genome').

%lost_option(annotate,sequenceid,[min,max],'Define a range to extract a sub-part of the genome').

% Input Format Specification
lost_input_formats(annotate,[text(fna),text(gbk)]).
lost_input_formats(parse,[text(fna)]).
% Output Format Specification
lost_output_format(annotate,_,text(prolog(sequence(dna)))).
lost_output_format(parse,_,text(prolog(sequence(dna)))).


annotate([FNA_File,GBK_File],Options,OutputFile) :-
        consult(fna_parser),
        open(GBK_File,read,GBK_Stream),
        get_info_gbk(GBK_Stream,GeneBank_Key,N_BP),
        close(GBK_Stream),
        parser_fna(Options,FNA_File,GeneBank_Key,N_BP,OutputFile).

parse([FNA_File],Options,OutputFile) :-
        maxint(Max),
        consult(fna_parser),
        parser_fna(Options,FNA_File,'n/a',Max,OutputFile).
