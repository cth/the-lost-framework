:- task(cluster_rna_foldings(text(prolog(ranges(_))),[],text(newick))).

:- use(lists).

% This needs to point to a B-Prolog installation.
% PRISM (with CHR) may be used in place of B-Prolog..
% ...but should be based on at least B-Prolog 7.7 to run this efficiently.
config(bprolog_with_chr,'/opt/BProlog/bp leuven_chr.out -g ').

%% cluster_rna_foldings(+InputFiles,+Options,+OutputFile)
% == 
% InputFiles = [InputFile]
% ==
% This tasks creates an hierarchical clustering of RNA foldings.
cluster_rna_foldings([InputFile],Options,OutputFile) :-
	get_option(Options,min_stem_length,MinStemLength),
	get_option(Options,max_pairing_mismatch,MaxPairMismatch),
	create_alignments(InputFile,AlignmentsFile,
		[minimal_stem_length(MinStemLength),
		pairing_count_mismatch_threshold(MaxPairMismatch)]
		),
	sort_alignments(AlignmentsFile),
	clusterings_from_alignments(AlignmentsFile,OutputFile).

%% create_alignments(+InputFile,+AlignmentsFile,+Constraints)
create_alignments(InputFile,AlignmentsFile,Constraints) :-
	map(fact_assertion,Constraints,AssertConstraints),
	map(term2atom,AssertConstraints,AtomAssertConstraints),
	intersperse(',',AtomAssertConstraints,AtomAssertConstraintsSeparated),	
	config(bprolog_with_chr,PrologWithCHR),
	lost_tmp_file('rna_alignments',AlignmentsFile),
	CmdListNested = [
		PrologWithCHR, 
		' "',
		'cl(rna_cluster), ',
		AtomAssertConstraintsSeparated,
		',',
		'create_alignments(\'',InputFile,'\',\'', AlignmentsFile, '\'),', 
		'halt.',
		'"',
	]
	flatten(CmdListNested,CmdList),
	atom_concat_list(CmdList,InvokeCommand),
	system(InvokeCommand),
	check_or_fail(file_exists(AlignmentsFile), 'alignments file not produced!').
	% where
	fact_assertion(Fact,assert(Fact)).

%% sort_alignments(+AligmentsFile)
sort_alignments(AlignmentsFile)
	terms_from_file(AlignmentsFile,Alignments),
	length(Alignments,NumAligns),
	write(NumAligns), write(' alignments..'),nl,
	writeln('Sorting alignments (may take some time)'),
	sort(Alignments,SortedAlignments),
	writeln(done),
	terms_to_file(AlignmentsFile,SortedAlignments).

%% clustering_from_alignments(+AlignmentsFile,+ClustersFile)
clustering_from_alignments(AlignmentsFile,ClustersFile) :-
	config(bprolog_with_chr,PrologWithCHR),
	CmdList = [
		PrologWithCHR, 
		'"cl(rna_cluster), ',
		'slink_cluster_by_distances(\'',AlignmentsFile,'\',\'', ClustersFile , '\'),', 
		'halt."'
	],
	atom_concat_list(CmdList,InvokeCmd),
	system(InvokeCmd).
