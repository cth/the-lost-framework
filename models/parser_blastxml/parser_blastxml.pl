%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NAME :
%      parser_blastxml.pl -- LOST tool v0.0
%
% FUNCTION :
%        Extraction of information from XML report of Blast
%
% HISTORIQUE :
%      M.P   31/03/2010: Creation
%
%  
% DESCRIPTION :
%         Generation of information for each HSP (hit) of the blast report
%         Format of the db:
%         blast(Blast_Output_Query_Def,Hit_Accession,Hsp_info)
%         Hsp_info:
%             [Hsp_bit_score
%              Hsp_score
%              Hsp_evalue
%              Hsp_query_from
%              Hsp_query_to
%              Hsp_hit_from
%              Hsp_hit_to
%              Hsp_pattern-from?
%              Hsp_pattern-to?
%              Hsp_query_frame?
%              Hsp_hit_frame?
%              Hsp_identity?
%              Hsp_positive?
%              Hsp_gaps?
%              Hsp_positive?
%              Hsp_align-len?
%              Hsp_density?
%              Hsp_qseq
%              Hsp_hseq
%              Hsp_midline]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ['../../lost.pl'].
:- lost_include_api(xml).

%---------
% Parser of XML blast report
%---------
% parser_blastxml(++XML.pl_File,--Output_File)
%----------
% Input file: pl file generated by the XML parser
% Output: Generation of the database (one fact by HSP)

parser_blastxml(XML2PL_File,Output_File) :-
        consult(XML2PL_File),
        open(Output_File,write,Stream_Output),
        open('blast_parser_log.txt',write,Stream_Log),
	 xml(_,[element(_Node_Name,[],BlastOutput)]),
        member(element('BlastOutput_iterations',_,BlastOutput_iterations),BlastOutput), 
        member(element('Iteration',_,Iteration),BlastOutput_iterations), 
        (member(element('Iteration_message',_,[pcdata(Message)]),Iteration) -> % No Hit Found
            atom_codes(Mess_Atom,Message),
            write(Mess_Atom),nl,
            member(element('Iteration_query-def',_,[pcdata(Query_Def)]),Iteration),
            append([39|Query_Def],[39],Query_Def2),
            atom_codes(Query_Def_Atom,Query_Def2),          
            write(Stream_Output,blast(Query_Def_Atom,'n/a',[])),write(Stream_Output,'.'),
            nl(Stream_Output)
        ;
            member(element('Iteration_query-def',_,[pcdata(Query_Def)]),Iteration),
	     append([39|Query_Def],[39],Query_Def2),
            member(element('Iteration_hits',_,Iteration_hits),Iteration),
            check_goal(
		findall(Hit,member(element('Hit',_,Hit),Iteration_hits),List_Hits)
		,
		'Fejl i findall'),!,
	     atom_codes(Query_Def_Atom,Query_Def2),
            write_hits(Stream_Output,Query_Def_Atom,List_Hits)
        ),
	 close(Stream_Log),
        close(Stream_Output).

check_goal(G,E):-
	( 
	call(G),
	!
	;
	writeln(E),
	fail
	).



write_hits(_Stream_Output,_Query_Def_Atom,[]) :-
        !.


write_hits(Stream_Output,Query_Def_Atom,[Hit|Rest_Hits]) :-
        !,
        member(element('Hit_accession',_,[pcdata(Hit_accession)]),Hit),
        member(element('Hit_hsps',_,Hit_hsps),Hit),
        findall(Hsp,member(element('Hsp',_,Hsp),Hit_hsps),List_Hsps),
        append([39|Hit_accession],[39],Hit_accession2),
        atom_codes(Hit_accession_atom,Hit_accession2),
        write_hsp(Stream_Output,Query_Def_Atom,Hit_accession_atom,List_Hsps),
        write_hits(Stream_Output,Query_Def_Atom,Rest_Hits).


write_hsp(_Stream_Output,_Query_Def_Atom,_Hit_Accession_Atom,[]) :-
        !.

write_hsp(Stream_Output,Query_Def_Atom,Hit_Accession_Atom,[Hsp|Rest_Hsp]) :-
        !,
        findall([Type,PcData],member(element(Type,_,[pcdata(PcData)]),Hsp),Hsp_Data),
        hsp2number(Hsp_Data,Hsp_Data2),
        Data =.. [blast,Query_Def_Atom,Hit_Accession_Atom,Hsp_Data2],
        write(Stream_Output,Data),
        write(Stream_Output,'.'),
        nl(Stream_Output),
        write_hsp(Stream_Output,Query_Def_Atom,Hit_Accession_Atom,Rest_Hsp).




hsp2number([],[]) :-
        !.

hsp2number([[Functor,List_Codes]|Rest_List],[Data|Rest]) :-
        catch(number_codes(Num,List_Codes),_,true),
        (var(Num) ->
            T = List_Codes
        ;
            T = Num
        ),
        atom_codes(Functor,Codes),
        append([39|Codes],[39],Codes2),
        atom_codes(Functor_Atom,Codes2),
        Data =.. [Functor_Atom,T],
        hsp2number(Rest_List,Rest).
        
        
        









