:- ['../lost.pl'].
:- lost_include_api(interface).

parser_ptt(PTT_File,ParsedPTTFile) :-
        run_model(parser_ptt, % Name of model
                  annotate([PTT_File], % A list of Input Files
                           [],          % Options
                           ParsedPTTFile)). % Output File

% Helper predictate 
filter_genes(ParsedPTT,RawGenome,Options,Filtered) :-
        run_model(gene_filter,annotate([ParsedPTT,RawGenome], Options,Filtered)).

filter_y_genes(Genes,Sequence,Filtered) :-
	Options = [ regex_no_match_extra_fields([ gene_name('^y.*$') ]) ],
	filter_genes(Genes,Sequence,Options,Filtered),
	write('output is :'),
	write(Filtered),
        nl.

filter_non_y_genes(Genes,Sequence,Filtered) :-
	Options = [ regex_match_extra_fields([ gene_name('^y.*$') ]) ],
	filter_genes(Genes,Sequence,Options,Filtered),
	write('output is :'),
	write(Filtered),
        nl.

filter_non_dir_frame_genes(Dir,Frame,Genes,Sequence,Filtered):-
	Options = [ match_frames([Frame]), match_strands([Dir]) ],
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
        filter_uncertain_genes(Genes,Genome,[],Certain),
        write('Results written to file: '), write(Certain), nl,

        % Filter y-genes
        write('Filtering genes which from the description seem uncertain:'),nl,
        filter_uncertain_genes(Genes,Genome,[],Certain),
        write('Results written to file: '), write(Certain), nl.


% For Ecoli
filter_genes_range(RangeMin,RangeMax,Filtered) :-
        Options = [range([RangeMin,RangeMax])],
        lost_sequence_file('U00096',Genome),
        lost_sequence_file('U00096_ptt',PTT),
	parser_ptt(PTT,Genes),
        run_model(gene_filter,annotate([Genes,Genome], Options,Filtered)).
        
