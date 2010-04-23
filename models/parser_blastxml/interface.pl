:- ['../../lost.pl'].
:- lost_include_api(interface). 
:- lost_include_api(xml).

% Generate a file of predicate based on Blast XML report.

	    
% Input Format Specification
lost_input_formats(lost_best_annotation,[text(xml(_))]).
% Output Format Specification
lost_output_format(lost_best_annotation,_,text(prolog(blast_hit))).


lost_best_annotation([XML_File],Options,OutputFile) :-
        consult('parser_blastxml.pl'),
        xml2pl(XML_File,'blast_pl.pl'),
        parser_blastxml('blast_pl.pl',OutputFile).
