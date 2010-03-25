% Produce a list of hard to find genes in the ecoli genome

:- ['../lost.pl'].
:- lost_include_api(interface).

minitest :-
	lost_sequence_file(gb_fragment,GBFile), % Genbank (reference genes)
	lost_sequence_file(eg_fragment,EGFile), % Easygene predictions
	lost_sequence_file(gm_fragment,GMFile), % Genemark predictions
	get_annotation_file(hard_to_find_genes,
			    [GBFile,EGFile,GMFile],
			    [option(file_functor(GBFile),gb),
			     option(file_functor(EGFile),eg),
			     option(file_functor(GMFile),gm)],
			    OutputFile),
	write('File written to: '), write(OutputFile), nl.


test :-
        lost_sequence_file('U00096_ptt',GenbankFile),
        get_annotation_file(parser_ptt, [GenbankFile], [], GBFile),
	lost_sequence_file('U00096_fna',SeqFile),
	get_annotation_file(genemark,[SeqFile],[parameters('Escherichia_coli_K12')],GMAllFile),
	get_annotation_file(best_prediction_per_stop_codon,
			    [GMAllFile],
			    [prediction_functor(genemark_gene_prediction),score_functor(start_codon_probability)],
			    GMBestFile),
	get_annotation_file(glimmer3,[SeqFile],[mode(from-scratch)],GlimFile),
        get_annotation_file(hard_to_find_genes,
			    [GBFile,GMBestFile,GlimFile],
			    [option(file_functor(GBFile),gb),
			     option(file_functor(GMBestFile),genemark_gene_prediction),
			     option(file_functor(GlimFile),glimmer3_gene_prediction)],
			    Hard2FindGenesFile),
	write('Find list of hard to find genes in: '), write(Hard2FindGenesFile),nl.


