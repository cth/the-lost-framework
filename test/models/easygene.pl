:- use(script).
:- use(prologdb).
:- use(fileformat).

% Fetch report from binf servers:
report(easygene) <-
	file::get('http://servers.binf.ku.dk/cgi-bin/easygene/output?id=60&format=gff&desc=CDS&desc=ORF&desc=ALT&r_cutoff=2&start=&stop=&strand=plus&strand=minus&per_page=100&183=Submit+Query').
	
parsed_report <- easygene::parse(report(easygene)).

testcase(easygene_parse) :-
	run(parsed_report),
	get_result_file(parsed_report,File),
	file_exists(File),
	check_format(text(prolog(ranges(gene))),File).
%	terms_from_file(File,Terms). % File is in text(prolog(ranges(gene))) format, so should be Prolog parsable.
	

