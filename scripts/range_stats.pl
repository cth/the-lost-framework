% Test the range stats model

:- ['../lost.pl'].
:- lost_include_api(interface).

range_stat_options([amino_acid_stats(no),length_stats(no),max_nucleotide_order(2),min_nucleotide_order(2)]).
%range_stat_options([amino_acid_stats(no),length_stats(no),max_nucleotide_order(5)]).
%range_stat_options([amino_acid_stats(no),length_stats(no),max_nucleotide_order(5)]).

gene_stats :-
        lost_sequence_file('U00096',GenomeFile),
        lost_sequence_file('U00096_ptt',PTTFile),
        parser_ptt(PTTFile,GenesFile),
        range_stat_options(Options),
	get_annotation_file(range_stats,[GenesFile,GenomeFile],Options,WithStats),
        write('file with stats written to: '),
        write(WithStats),
        nl.

genome_stats :-
       lost_sequence_file('U00096',GenomeFile),
       lost_tmp_directory(TmpDir),
       atom_concat(TmpDir, 'my_tmp_file.pl',TempFile),
       terms_to_file(TempFile,[genome(1,100000,1,'+',[])]),
        range_stat_options(Options),
       get_annotation_file(range_stats,[TempFile,GenomeFile],Options,WithStats),
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

