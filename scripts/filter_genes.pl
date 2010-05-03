:- ['../lost.pl'].
:- lost_include_api(interface).

parser_ptt(Name_PTT_File,ParsedPTTFile) :-
        lost_sequence_file(Name_PTT_File,PTT_File),
        get_annotation_file(parser_ptt, % Name of model
                            [PTT_File], % A list of Input Files
                            [],          % Options
                            ParsedPTTFile), % Output File
        write('Parsing succeeds!! see.: '),
        write(ParsedPTTFile),
	nl.

filter_genes(ParsedPTT,Filtered) :-
	lost_sequence_file('U00096',RawGenome),
	get_annotation_file(gene_filter,
	[ParsedPTT,RawGenome],
	[	
		match_extra_fields([gene_name('^y.*$')])
	],
	Filtered).

t :-
	parser_ptt('U00096_ptt',PTTParsed),
	filter_genes(PTTParsed,Filtered),
	write('output is :'),
	write(Filtered),
	nl.

	


