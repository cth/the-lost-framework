:- use(script).
:- use(prologdb).
:- use(fileformat).

% Load a short plasmid file, just for testing..
fasta_file <- file::get('ftp://ftp.ncbi.nlm.nih.gov/genomes/Plasmids/fna/NC_000843.fna').

candidate_orfs <- pyl::candidate_orfs(fasta_file).

candidate_orfs_w_seqid <- pyl::candidate_orfs([fasta_file],[sequence_identifier('NC_000843')]).

testcase(candidate_orfs) :-
	Target = candidate_orfs,
	rerun(Target),!,
	get_result_file(Target,File),!,
	file_exists(File),!,
	check_format(text(prolog(ranges(gene))),File).

testcase(candidate_orfs_with_sequence_id) :-
	Target = candidate_orfs_w_seqid,
	rerun(Target),!,
	get_result_file(Target,File),!,
	file_exists(File),!,
	check_or_fail(
		check_format(text(prolog(ranges(gene))),File),
		'format is incorrect'),
	terms_from_file(File,GeneTerms),!,
	check_or_fail(
		forall(member(G,GeneTerms), gene_sequence_id(G,'NC_000843')),
		'sequence ids doesnt match').
