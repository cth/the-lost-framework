% Testcases for glimmer model

:- use(script).
:- use(prologdb).
:- use(fileformat).

% Fetch genome file
fasta_file <- file::get('ftp://ftp.ncbi.nlm.nih.gov/genomes/Bacteria/Escherichia_coli_K_12_substr__MG1655_uid57779/NC_000913.fna')

report(from_scratch) <- glimmer3::annotate(fasta_file, [mode(from-scratch)]).

testcase(glimmer_annotate) :-
	run(report(from_scratch)),
	get_result_file(Target,File),
	file_exists(File),
	check_format(text(prolog(ranges(gene))),File).

