% Test integration of various genefinders.

:- ['../lost.pl'].
:- lost_include_api(interface).

test_genemark :-
	lost_sequence_file('U00096_fna',SeqFile),
	run_model(genemark,
                  annotate([SeqFile],[parameters('Escherichia_coli_K12')],OutFile)),
   	write('Results are stored in: '),
	write(OutFile),
	nl.

test_glimmer :-
	lost_sequence_file('U00096_ptt',PttFile),
	run_model(parser_ptt, annotate([PttFile],[],GBFilePl)),
	lost_sequence_file('U00096_fna',SeqFile),
	run_model(glimmer3,g3_from_scratch([SeqFile],[mode(from-scratch)],PredFile)),
	write('Results are stored in: '),
	write(PredFile),
        run_model(accuracy_report,annotate([GBFilePl,PredFile],[],Acc)),
	nl.
