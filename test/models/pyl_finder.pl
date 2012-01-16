:- use(script).
:- use(prologdb).
:- use(fileformat).

% Load a short plasmid file, just for testing..
fasta_file <- file::get('ftp://ftp.ncbi.nlm.nih.gov/genomes/Plasmids/fna/NC_000843.fna').

candidate_orfs <- pyl_finder::candidate_orfs(fasta_file).
	
testcase(candidate_orfs) :-
	Target = candidate_orfs,
	rerun(Target),
	get_result_file(Target,File),
	file_exists(File),
	check_format(text(prolog(ranges(gene))),File).

