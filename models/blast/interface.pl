:- task(
	makeblastdb(
		[test(fasta)],
		[
			dbtype(nucl), % 
			parse_seqids(true),
			title(blast_database)
		],
		blast_database)).
		
:- task(
	tblastn(
			[test(fasta)],
			[
				evalue('10e-6'), %
				outfmt(5),
				soft_masking(true),
				db_gencode(11)
			],
			blast)).
			
:- task(parse_xml([xml(blast)],[],text(prolog(ranges(gene))))).

:- task(remove_self_hits([text(prolog(ranges(gene)))],[],text(prolog(ranges(gene))))).
			
%% makeblastdb(+InputFiles,+Options,+OutputFile)
% calls the =|makeblastdb|= utility to create a database for searching with blast
makeblastdb([InputFile],Options,OutputFile) :-
	to_blast_opts([in(InputFile),out(OutputFile)|Options],BlastOpts),
	atom_concat_list(['makeblastdb '|BlastOpts],BlastCommand),
	terms_to_file(OutputFile,['This is not really the blast database. The real database files are extentions of this filename']),
    writeln(BlastCommand),
	system(BlastCommand).

%% tblastn(+InputFiles,+Options,+OutputFile)
% ==
% InputFiles = [DatabaseFile,QueriesFile]
% ==
% Runs =|tblastn|= with to search =|DatabaseFile|= with each of the queries from =|QueriesFile|=.
tblastn([DatabaseFile,QueriesFile],Options,OutputFile) :-
	to_blast_opts([db(DatabaseFile),out(OutputFile),query(QueriesFile)|Options],BlastOpts),
	atom_concat_list(['tblastn'|BlastOpts],BlastCommand),
	writeln(BlastCommand),
	system(BlastCommand).
	
%% parse_xml(+InputFiles,+Options,+OutputFile)
% == 
% InputFiles = [BlastXML]
% ==
% This uses the ruby script =|blastxml2prolog_sax.rb|= which relies on Ruby and Nokogiri.
parse_xml([BlastXML],Options,OutputFile) :-
	atom_concat_list(['ruby ', 'blastxml2prolog_sax.rb ',BlastXML,' > ', OutputFile],ParseCmd),
	writeln(ParseCmd),
	system(ParseCmd).
	
%% remove_self_hits(+InputFiles,+Options,+OutputFile)
% == 
% InputFile = [BlastResults]
% ==
remove_self_hits([BlastResultFile],_Options,OutputFile) :-
	use(genedb),
	open(BlastResultFile,read,InStream),
	open(OutputFile,write,OutStream),
	remove_self_hits_rec(InStream,OutStream),
	close(InStream),
	close(OutStream).
	
remove_self_hits_rec(InStream,OutStream) :-
	read(InStream,Term),
	write('.'),
	((Term == end_of_file) ->
		true
		;
		gene_left(Term,Left),
		gene_right(Term,Right),
		gene_strand(Term,Strand),
		gene_extra_field(Term,hit_strand,HitStrand),
		gene_extra_field(Term,hit_left,HitLeft),
		gene_extra_field(Term,hit_right,HitRight),
		((Left =< HitLeft, Right >= HitRight, Strand == HitStrand) ->
			true 
			;
			writeq(OutStream,Term),
			write(OutStream,'.\n')
			),
		!,
		remove_self_hits_rec(InStream,OutStream)).

%% Utilities

to_blast_opts([],[]).
	
%to_blast_opts([O|Os],[B|Bs]) :-
%	O =.. [ Key, true ],
%	!,
%	atom_concat(' -',Key,B),
%	to_blast_opts(Os,Bs).

to_blast_opts([O|Os],[B|Bs]) :-
	O =.. [ Key, Value ],
	atom_concat_list([' -',Key,'=',Value],B),
	to_blast_opts(Os,Bs).
	
