% Testing different selection criteria

:- ['../lost.pl'].
:- lost_include_api(interface).

test_score :-
        lost_sequence_file('U00096_ptt',GenbankFile),
        get_annotation_file(parser_ptt, [GenbankFile], [], GBFile),
	lost_sequence_file('U00096_fna',SeqFile),
	get_annotation_file(genemark,[SeqFile],[parameters('Escherichia_coli_K12')],GMAllFile),
	get_annotation_file(best_prediction_per_stop_codon,
			    [GMAllFile],
			    [prediction_functor(genemark_gene_prediction),score_functor(start_codon_probability)],
			    GMBestFile),
	get_annotation_file(accuracy_report,
			    [GBFile,GMBestFile],
			    [
			     reference_functor(gb),
			     prediction_functor(genemark_gene_prediction),
			     start(1),
			     end(4639651)
			    ],
			    OutputFile),
	write('Displaying output file: '),write(OutputFile),nl,
	readFile(OutputFile,Contents),
	atom_codes(Atom,Contents),
	write(Atom),
	nl.

test_longest :-
        lost_sequence_file('U00096_ptt',GenbankFile),
        get_annotation_file(parser_ptt, [GenbankFile], [], GBFile),
	lost_sequence_file('U00096_fna',SeqFile),
	get_annotation_file(genemark,[SeqFile],[parameters('Escherichia_coli_K12')],GMAllFile),
	get_annotation_file(longest_prediction_per_stop_codon,
			    [GMAllFile],
			    [prediction_functor(genemark_gene_prediction),score_functor(start_codon_probability)],
			    GMBestFile),
	get_annotation_file(accuracy_report,
			    [GBFile,GMBestFile],
			    [
			     reference_functor(gb),
			     prediction_functor(genemark_gene_prediction),
			     start(1),
			     end(4639651)
			    ],
			    OutputFile),
	write('Displaying output file: '),write(OutputFile),nl,
	readFile(OutputFile,Contents),
	atom_codes(Atom,Contents),
	write(Atom),
	nl.