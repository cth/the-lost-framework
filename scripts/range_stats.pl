% Test the range stats model

:- ['../lost.pl'].
:- lost_include_api(interface).

test :-
        lost_sequence_file('U00096',GenomeFile),
        lost_sequence_file('U00096_ptt',PTTFile),
        parser_ptt(PTTFile,GenesFile),
	get_annotation_file(range_stats,[GenesFile,GenomeFile],[amino_acid_stats(no),length_stats(no),max_nucleotide_order(5)],WithStats),
        write('file with stats written to: '),
        write(WithStats),
        nl.

parser_ptt(PTT_File,ParsedPTTFile) :-
        get_annotation_file(parser_ptt, % Name of model
                            [PTT_File], % A list of Input Files
                            [],          % Options
                            ParsedPTTFile), % Output File
        write('Parsing succeeds!! see.: '),
        write(ParsedPTTFile),
	nl.

