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

% Helper predictate 
filter_genes(ParsedPTT,RawGenome,Options,Filtered) :-
	get_annotation_file(gene_filter,[ParsedPTT,RawGenome], Options,Filtered).

filter_y_genes(Genes,Sequence,Filtered) :-
	Options = [ regex_no_match_extra_fields([ gene_name('^y.*$') ]) ]
	filter_genes(Genes,Sequence,Options,Filtered),
	write('output is :'),
	write(Filtered),
        nl.

filter_non_y_genes(Genes,Sequence,Filtered) :-
	Options = [ regex_match_extra_fields([ gene_name('^y.*$') ]) ]
	filter_genes(Genes,Sequence,Options,Filtered),
	write('output is :'),
	write(Filtered),
        nl.

% Exclude 'predicted/hypothetical/putative' etc genes
% The list of word is taken from easygene paper
filter_uncertain_genes(Genes,Genome,ExtraOptions,Filtered) :-
	Options = [
	        regex_no_match_extra_fields([
			product("^.*(predicted|putative|unknown|possible|hypothetical|probable|bacteriophage|transposon|insertion|reverse transcriptase).*$")
	        ])
	],
	filter_genes(Genes,Sequence,Options,Filtered),
	write('output is :'),
	write(Filtered),
        nl.


run_ecoli_filters :-
	lost_sequence_file('U00096',Genome),
        lost_sequence_file('U00096_ptt',PTT),
	parser_ptt(PTT,Genes),

        % Filter uncertain genes
        write('Filtering genes which from the description seem uncertain:'),nl,
        filter_uncertain_genes(Genes,Genome,Certain),
        write('Results written to file: '), write(Certain), nl,

        % Filter y-genes
        write('Filtering genes which from the description seem uncertain:'),nl,
        filter_uncertain_genes(Genes,Genome,Certain),
        write('Results written to file: '), write(Certain), nl.
