:- ['../../lost.pl'].
:- lost_include_api(misc_utils).

get([URL],_Options,OutputFile) :-
	build_command(URL,OutputFile,Cmd),
	writeln(Cmd),
	system(Cmd).
	
build_command(URL,OutputFile,Cmd) :-
	atom_concat_list([wget, ' ', URL, ' --output-document=', OutputFile],Cmd).
	
test :-
	get(['ftp://ftp.ncbi.nih.gov/genomes/Bacteria/Escherichia_coli_K_12_substr__MG1655_uid57779/NC_000913.Prodigal-2.50'],[],'test.out').