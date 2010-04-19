:- ['../../lost.pl'].
:- lost_include_api(interface). 
:- lost_include_api(xml).

% Generate a file of predicate based on Easygene report.

lost_best_annotation([XML_File],Options,OutputFile) :-
        consult('parser_blastxml.pl'),
        xml2pl(XML_File,'blast_pl.pl'),
        parser_blastxml('blast_pl.pl',OutputFile).
%%%         lost_config(platform,Platform)
%%%         (Platform = 'windows' ->
%%%             system('del blast_pl.pl')
%%%         ;
%%%             system('rm blast_pl.pl')
%%%         ).
