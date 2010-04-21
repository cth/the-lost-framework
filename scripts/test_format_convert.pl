:- ['../lost.pl'].
:- lost_include_api(interface).

fasta2prolog(OutputFile) :-
	lost_data_file('U00096_fna',File),
	get_annotation_file(format_converter,
			    [File],
			    [input_format(text(fasta(_))), output_format(text(prolog(sequence(_))))],
			    OutputFile),
	write('prolog output written to: '),
	write(OutputFile),
	nl.

test_prolog2fasta :-
	fasta2prolog(PrologFile),
	get_annotation_file(format_converter,
			    [PrologFile],
			    [input_format(text(prolog(sequence(_)))), output_format(text(fasta(_)))],
			    OutputFile),
	write('fasta output written to: '),
	write(OutputFile),
	nl.

test :- test_prolog2fasta.

