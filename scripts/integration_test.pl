% Test integration of various genefinders.

:- ['../lost.pl'].
:- lost_include_api(interface).

test_genemark :-
	lost_sequence_file('U00096_fna',SeqFile),
	get_annotation_file(genemark,
                            [SeqFile],
			    [parameters('Escherichia_coli_K12')],
			    OutFile),
	write('Results are stored in: '),
	write(OutFile),
	nl.

test_glimmer :-
	lost_sequence_file('U00096_fna',SeqFile),
	get_annotation_file(glimmer3,[SeqFile],[mode(from-scratch)],OutFile),
	write('Results are stored in: '),
	write(OutFile),
	nl.
