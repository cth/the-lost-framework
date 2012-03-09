:- task(
	makeblastdb(
		[test(fasta)],
		[
			dbtype(nucl), % 
			parse_seqids(true),
			title(blast_database)
		],
		blast_database)).

%% makeblastdb(+InputFiles,+Options,+OutputFile)
% calls the =|makeblastdb|= utility to create a database for searching with blast
makeblastdb([InputFile],Options,OutputFile) :-
	to_blast_opts([in(InputFile),out(OutputFile)|Options],BlastOpts),
	atom_concat_list(['makekblastdb '|BlastOpts],BlastCommand),
	system(BlastCommand).
	
%% Utilities

to_blast_opts([],[]).
	
to_blast_opts([O|Os],[B|Bs]) :-
	O =.. [ Key, true ],
	!,
	atom_concat(' -',Key,B),
	to_blast_opts(Os,Bs).

to_blast_opts([O|Os],[B|Bs]) :-
	O =.. [ Key, Value ],
	atom_concat_list([' -',Key,'=',Value],B),
	to_blast_opts(Os,Bs).
	
