:- ['../../lost.pl'].
:- lost_include_api(xml).
%:- lost_include_api(dynamic_consult).
:- lost_include_api(io).

%---------
% Parser of XML blast report
%---------
/*
blast_hit_facts(BlastXMLFile,HitFacts) :-
	lost_tmp_directory(TmpDir),
	%atom_concat(TmpDir,'parsed_blast_xml.pl',BlastPrologFile),
	BlastPrologFile = 'parsed_blast_xml.pl',
    xml2pl(BlastXMLFile,BlastPrologFile),
	dynamic_consult(BlastPrologFile),
	findall(F,parse_xml(F),FF),!,
	write(FF),nl,
	write(here),nl,!,
	dynamic_unconsult(BlastPrologFile).
*/
blast_hit_facts(BlastXMLFile,HitFacts) :-
	lost_tmp_directory(TmpDir),
	BlastPrologFile = 'parsed_blast_xml.pl',
	write('parsing xml file...'),
    xml2pl(BlastXMLFile,BlastPrologFile),
	write(done),nl,
%	[BlastPrologFile],
%	xml(_,[element('BlastOutput',[],BlastOutput)]),
	terms_from_file(BlastPrologFile,Terms),
	Terms=[xml(_,[element('BlastOutput',[],BlastOutput)])],
	write(here),nl,
	findall(F,parse_blast_output_xml(BlastOutput,F),HitFacts),!.

parse_blast_output_xml(BlastOutput,HitFacts) :-
	member(element('BlastOutput_iterations',_,BlastOutput_iterations),BlastOutput), 
	member(element('Iteration',_,Iteration),BlastOutput_iterations),
	(member(element('Iteration_message',_,[pcdata(Message)]),Iteration) -> % No Hit Found
		atom_codes(Mess_Atom,Message),
		write(Mess_Atom),nl,
		member(element('Iteration_query-def',_,[pcdata(Query_Def)]),Iteration),!,
		append([39|Query_Def],[39],Query_Def2),
		atom_codes(Query_Def_Atom,Query_Def2),
		HitFacts = [blast_hit(Query_Def_Atom,'n/a',[])]
	;
		write('found hit'),nl,
		member(element('Iteration_query-def',_,[pcdata(Query_Def)]),Iteration),!,
 		append([39|Query_Def],[39],Query_Def2),
		member(element('Iteration_hits',_,Iteration_hits),Iteration),!,
		check_goal(findall(Hit,member(element('Hit',_,Hit),Iteration_hits),List_Hits), 'Fejl i findall'),!,
		atom_codes(Query_Def_Atom,Query_Def2),
%		write(build_hits(Query_Def_Atom,List_Hits,HitFacts)),nl,
		write('query def atom: '), write(Query_Def_Atom),nl,
		build_hits(Query_Def_Atom,List_Hits,HitFacts),!
	).

check_goal(G,E):-
	( 
	call(G),
	!
	;
	writeln(E),
	fail
	).


build_hits(_Query_Def_Atom,[],[]).

build_hits(Query_Def_Atom,[Hit|Rest_Hits],[HitsFacts|HitsFactsRest]) :-
		member(element('Hit_accession',_,[pcdata(Hit_accession)]),Hit),
		member(element('Hit_hsps',_,Hit_hsps),Hit),
		% Find all HSP's
		findall(Hsp,member(element('Hsp',_,Hsp),Hit_hsps),List_Hsps),
		append([39|Hit_accession],[39],Hit_accession2),
		atom_codes(Hit_accession_atom,Hit_accession2),
		build_hsp(Query_Def_Atom,Hit_accession_atom,List_Hsps,HitsFacts),
%		write(build_hits_rec(Query_Def_Atom,Rest_Hits,HitsFactsRest)),nl,
		build_hits(Query_Def_Atom,Rest_Hits,HitsFactsRest).

build_hsp(_Query_Def_Atom,_Hit_Accession_Atom,[],[]).

build_hsp(Query_Def_Atom,Hit_Accession_Atom,[Hsp|Rest_Hsp],[HspFact|HspFactRest]) :-
	!,
	findall([Type,PcData],member(element(Type,_,[pcdata(PcData)]),Hsp),Hsp_Data),
%	write('HSP_DATA:'),nl,
%	write(Hsp_Data),nl,
	hsp2number(Hsp_Data,Hsp_Data2),
	HspFact =.. [blast,Query_Def_Atom,Hit_Accession_Atom,Hsp_Data2],
% 	write(build_hsp(Query_Def_Atom,Hit_Accession_Atom,Rest_Hsp,HspFactRest)),nl,
	build_hsp(Query_Def_Atom,Hit_Accession_Atom,Rest_Hsp,HspFactRest).

hsp2number([],[]).

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









