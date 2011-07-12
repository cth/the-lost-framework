:- ['../lost.pl'].

test :-
	lost_data_directory(DD),
	atom_concat(DD,'test_blast_trim.pl', BlastFile),
	run_model(stop_trimmer,trim_stops([BlastFile],[],OutFile)),
	write('outfile is :'), write(OutFile), nl.
	
small_blast_files(Files) :-
	FileNames = ['blast_results_NC_000913.xml.pl.100','blast_results_NC_004547.xml.pl.100',
		'blast_results_NC_008800.xml.pl.100','blast_results_NC_009436.xml.pl.100',
		'blast_results_NC_009792.xml.pl.100','blast_results_NC_010067.xml.pl.100',
		'blast_results_NC_010694.xml.pl.100','blast_results_NC_011283.xml.pl.100'],
	lost_data_directory(DD),
	findall(FullFileName,(member(FName,FileNames),atom_concat(DD,FName,FullFileName)),Files).

idseqs :-
	small_blast_files(Files),
	idseqs(Files,_).

idseqs(InputFiles,OutputFiles) :-
	findall(OutFile,
		(
			member(InFile,InputFiles),
			run_model(process_blast_matches,identity([InFile],[],OutFile)),
			write('outfile is :'), write(OutFile), nl
		),
		OutputFiles).

small_test_idseq :-
	lost_sequence_file('blast_results_problem',SeqFile),
	run_model(process_blast_matches,identity([SeqFile],[],OutFile)),
	write('outfile is :'), write(OutFile), nl.
	

merge_idseq :-
	lost_data_directory(DD),
	atom_concat(DD,'blast_identity1.seq', BlastFile1),
	atom_concat(DD,'blast_identity2.seq', BlastFile2),
	run_model(process_blast_matches,merge_identity([BlastFile1,BlastFile2],[],OutFile)),
	write('outfile is :'), write(OutFile), nl.


merge_multiple :-
	small_blast_files(Files1),
	idseqs(Files1,Files2),
	merge_multiple(Files2,_MergedFile).


merge_multiple(InputFiles,OutputFile) :-
	run_model(process_blast_matches,merge_multiple(InputFiles,[],OutputFile)),
	write('outfile is :'), write(OutputFile), nl.
	
		
sum_multiple :-
	small_blast_files(Files1),
	idseqs(Files1,Files2),
	sum_multiple(Files2,_MergedFile).

sum_multiple(InputFiles,OutputFile) :-
	run_model(process_blast_matches,sum_multiple(InputFiles,[],OutputFile)),
	write('outfile is :'), write(OutputFile), nl.
	

%% 
	
	

