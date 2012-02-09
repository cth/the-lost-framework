:- use(script).
:- use(prologdb).
:- use(fileformat).

% Load a short plasmid file, just for testing..
fasta_file <- file::get('ftp://ftp.ncbi.nlm.nih.gov/genomes/Plasmids/fna/NC_000843.fna').

candidate_pyl_orfs <- pyl_finder::candidate_orfs(fasta_file,[sequence_identifier('NC_000843')]).

candidate_pylis <- pyl_finder::candidate_pylis(candidate_pyl_orfs).

folded <- ppfold::fold(candidate_pylis, [sequence_functor(pylis)]).


clustered_alignments <- rna_cluster::cluster_rna_foldings(folded).

testcase(clustered_alignments) :-
	rerun(clustered_alignments),!,
	get_result_file(clustered_alignments,File),!,
	check_or_fail(file_exists(File),'file doesnt exist').
