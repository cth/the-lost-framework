:- use(script).
:- use(prologdb).
:- use(fileformat).

% Fetch report from binf servers:
prodigal_report <- file::get('ftp://ftp.ncbi.nlm.nih.gov/genomes/Bacteria/Escherichia_coli_K_12_substr__MG1655_uid57779/NC_000913.Prodigal-2.50').
	
parsed_report <- prodigal::parse(ptt_file).

testcase(easygene_parse) :-
	Target = parsed_report,
	run(Target),
	get_result_file(Target,File),
	file_exists(File),
	check_format(text(prolog(ranges(gene))),File).

