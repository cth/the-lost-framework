% Convert a set of genes (statistics) to CSV file 

:- ['./../lost.pl'].

:- lost_include_api(io).
:- lost_include_api(misc_utils).

test_run :-
        genestats_to_csv('../data/range_stats_28.gen','../data/range_stats_28.csv','../data/range_stats_28.lbl').

genestats_to_csv(InputFile,OutputFile,LabelsFile) :-
        terms_from_file(InputFile,GeneTerms),
        tell(OutputFile),
        terms_to_csv(GeneTerms),
        told,
        tell(LabelsFile),
        GeneTerms = [FirstTerm|_],
        terms_to_labels(FirstTerm),
        told.

tmp(A,B,C) :-
        write(tmp(A,B,C)),nl.

terms_to_csv([]).
terms_to_csv([GeneTerm|Rest]) :-
        %write(GeneTerm),nl,
        GeneTerm =.. [ _functor,_start,_end,_frame,_strand,Extra],

        % First is label
        (member(gene_name(GeneName), Extra) ->
                write(GeneName),
                write(',')
                ;
                true),

        (member(nucleotide_stats(NStats), Extra) ->
                sort(NStats,NStatsSorted),
                map(extract_stat_freq,NStatsSorted,Stats1)
                ;
                Stats1 = []
        ),

        (member(amino_acid_stats(AStats), Extra) ->
                sort(AStats,SortAStats),
                map(extract_stat_freq,SortAStats,AFreqs),
                append(Stats1,AFreqs,Stats2)
                ;
                Stats2 = Stats1
        ),
               

        % Gene length
        (member(normalized_gene_length(GeneLength), Extra) ->
                Stats3 = [GeneLength|Stats2]
                ;
                Stats3 = Stats2
        ),
        interleave_list_with(Stats3,',',Stats4),
        map(write(input),Stats4,_),
        nl,
        terms_to_csv(Rest).

% Write one label per line
terms_to_labels(GeneTerm) :-
        %write(GeneTerm),nl,
        GeneTerm =.. [ _functor,_start,_end,_frame,_strand,Extra],

        % First is label
        (member(gene_name(_), Extra) ->
                write(gene_name),nl
                ;
                true),

        (member(nucleotide_stats(NStats), Extra) ->
                sort(NStats,NStatsSorted),
                map(extract_stat_label,NStatsSorted,Stats1)
                ;
                Stats1 = []
        ),

        (member(amino_acid_stats(AStats), Extra) ->
                sort(AStats,SortAStats),
                map(extract_stat_label,SortAStats,AFreqs),
                append(Stats1,AFreqs,Stats2)
                ;
                Stats2 = Stats1
        ),
               

        % Gene length
        (member(normalized_gene_length(GeneLength), Extra) ->
                Stats3 = [GeneLength|Stats2]
                ;
                Stats3 = Stats2
        ),
        interleave_list_with(Stats3,'\n',Stats4),
        map(write(input),Stats4,_),
        nl.

extract_stat_freq(stat(_,Freq),Freq). 
extract_stat_label(stat(Label,_),Label).


interleave_list_with([],_,[]).
interleave_list_with([One],_,[One]).
interleave_list_with([One,Two|Rest],Interleave,[One,Interleave|NewRest]) :-
        interleave_list_with([Two|Rest],Interleave,NewRest).

