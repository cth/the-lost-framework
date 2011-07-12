% Testing different selection criteria

:- ['../lost.pl'].
:- lost_include_api(interface).

test_score :-
        lost_sequence_file('U00096_ptt',GenbankFile),
        run_model(parser_ptt, annotate([GenbankFile], [], GBFile)),
	lost_sequence_file('U00096_fna',SeqFile),
	run_model(genemark,annotate([SeqFile],[parameters('Escherichia_coli_K12')],GMAllFile)),
	run_model(best_prediction_per_stop_codon,
                  annotate([GMAllFile],
			   [prediction_functor(genemark_gene_prediction),score_functor(start_codon_probability)],
			   GMBestFile),
	run_model(accuracy_report,
		  annotate([GBFile,GMBestFile],
			   [
			     reference_functor(gb),
			     prediction_functor(genemark_gene_prediction),
			     start(1),
			     end(4639651)
			   ],
			   OutputFile)),
	write('Displaying output file: '),write(OutputFile),nl,
	readFile(OutputFile,Contents),
	atom_codes(Atom,Contents),
	write(Atom),
	nl.

test_longest :-
        lost_sequence_file('U00096_ptt',GenbankFile),
        run_model(parser_ptt, annotate([GenbankFile], [], GBFile)),
	lost_sequence_file('U00096_fna',SeqFile),
	run_model(genemark,annotate([SeqFile],[parameters('Escherichia_coli_K12')],GMAllFile)),
	run_model(longest_prediction_per_stop_codon,
		  annotate([GMAllFile],
			   [prediction_functor(genemark_gene_prediction),score_functor(start_codon_probability)],
			   GMBestFile)),
	run_model(accuracy_report,
	          annotate([GBFile,GMBestFile],
			   [
			    reference_functor(gb),
			    prediction_functor(genemark_gene_prediction),
			    start(1),
			    end(4639651)
			   ],
			   OutputFile)),
	write('Displaying output file: '),write(OutputFile),nl,
	readFile(OutputFile,Contents),
	atom_codes(Atom,Contents),
	write(Atom),
	nl.
