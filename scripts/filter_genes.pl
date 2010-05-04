:- ['../lost.pl'].
:- lost_include_api(interface).

parser_ptt(PTT_File,ParsedPTTFile) :-
        get_annotation_file(parser_ptt, % Name of model
                            [PTT_File], % A list of Input Files
                            [],          % Options
                            ParsedPTTFile), % Output File
        write('Parsing succeeds!! see.: '),
        write(ParsedPTTFile),
	nl.

filter_genes(ParsedPTT,RawGenome,Filtered) :-
	get_annotation_file(gene_filter,
	[ParsedPTT,RawGenome],
	[	

	],
%		match_extra_fields([gene_name('^z.*$')])			    
	Filtered).

t :-
%	lost_sequence_file('short',RawGenome),
%        lost_sequence_file('short_ptt',PTT_File),

	lost_sequence_file('U00096',RawGenome),
        lost_sequence_file('U00096_ptt',PTT_File),
	
	parser_ptt(PTT_File,ParsedPTT),
	filter_genes(ParsedPTT,RawGenome,Filtered),
	write('output is :'),
	write(Filtered),
	nl.

	


