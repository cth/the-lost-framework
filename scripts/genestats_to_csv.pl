% Convert a set of genes (statistics) to CSV file 

:- ['../../lost.pl'].

:- lost_include_api(io).

genes_to_csv(InputFile,OutputFile) :-
        terms_from_file(InputFile,GeneTerms),
        tell(OutputFile),
        terms_to_csv(GeneTerms),
        told.


terms_to_csv([]).
terms_to_csv([GeneTerm|Rest]) :-
        %write(GeneTerm),nl,
        GeneTerm =.. [ _functor,_start,_end,_frame,_strand,Extra],

        % First is label
        %member(gene_name(GeneName), Extra),
        %write(GeneName),
        %write(','),

        member(nucleotide_stats(NStats), Extra),
        % sort assures that they are always printed in same order
        sort(NStats,SortNStats), 
        forall(member(stat(_Type,Freq),SortNStats), (write(Freq),write(','))),

        member(amino_acid_stats(AStats), Extra),
        sort(AStats,SortAStats),
        forall(member(stat(_Type,Freq),SortAStats), (write(Freq),write(','))),

        % Gene length
        member(normalized_gene_length(GeneLen), Extra),
        write(GeneLen),
        nl,
        
        terms_to_csv(Rest).
