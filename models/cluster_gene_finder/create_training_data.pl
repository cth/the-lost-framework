% This script creates the training data for the 
% cluster gene model,
% It assumes,
% - A clustering of genes. 
% - A genome file containing the nucleotide sequence of the entire genome
% - A genes file

:- ['../../lost.pl'].
:- lost_include_api(io).


test :-
        create_training_data(   'clusters.pl',
                                'ecoli_genes.pl',
                                'ecoli_orfs.pl').

create_training_data(ClustersFile,GenesFile,ORFFile,TrainingDataFile) :-
        load_genes(GenesFile),
        load_clusters(ClustersFile),
        open(ORFFile,read,InStream),
        open(TrainingDataFile,write,OutStream),
        % Pretend to the model that there is one cluster (for genes)
        % We only need/want one for purposes of generating gene
        % statistics
        assert(coding_clusters(1)), 
        create_orf_training_data(InStream,OutStream),
        retract(coding_clusters(1)),
        close(InStream),
        close(OutStream).

create_orf_training_data(InStream,OutStream) :-
        read(InStream, Orf),
        ((Orf == end_of_file) ->
	 write('Finally: done'),nl
        ;
	 (gene_orf(Gene,Orf) ->
	  write(Gene), flush_output,
	  (orf_and_gene_to_learn_goal(Gene,Orf,LearnGoal) ->
	   write('!'), flush_output
	  ;
	   nl,write('Problem with gene: '), write(Gene), nl
	  )
	 ;
	  write('.'), flush_output,
	  non_coding_orf_to_learn_goal(Orf,LearnGoal)
	 ),

	 (ground(LearnGoal) ->
	  writeq(OutStream,LearnGoal),
	  write(OutStream,'.\n')
	 ;
	  true
	 ),
	 
	 !,
	 create_orf_training_data(InStream,OutStream)
	).


% Load clusters
load_clusters(ClustersFile) :-
        terms_from_file(ClustersFile,ClusterTerms),
        load_clusters_rec(ClusterTerms).

load_clusters_rec([cluster(Id,_,Genes)|Cs]) :-
        forall(member(Gene,Genes), assert(cluster_gene(Id,Gene))),
        load_clusters_rec(Cs).
load_clusters_rec([]).

% Load genes and assert facts with the functor "gene" 
% Each of these facts are then indexed on gene name (first argument)
load_genes(GenesFile) :-
        terms_from_file(GenesFile,GeneTerms),
        load_genes_rec(GeneTerms).
         
load_genes_rec([G|Gs]) :-
        G =.. [_,LeftEnd,RightEnd,Strand,Frame,Extra],
        member(gene_name(GeneName),Extra),
        assert(gene(GeneName,LeftEnd,RightEnd,Strand,Frame)),
        load_genes_rec(Gs).
load_genes_rec([]).

% Assumes that gene facts are available
gene_orf(GeneName,Orf) :-
        Orf =.. [_,_,LeftEnd,RightEnd,Strand,Frame,_], 
        ((Strand =='+') ->
                gene(GeneName,_,RightEnd,Strand,Frame)
                ;
                gene(GeneName,LeftEnd,_,Strand,Frame)
        ).

% Assumes that all ORFs are available
get_gene_orf(GeneName,chunk(Id,LeftEnd,RightEnd,Strand,Frame,Extra)) :-
        gene(GeneName,LeftEnd,RightEnd,Strand,Frame),
        chunk(Id,LeftEnd,RightEnd,Strand,Frame,Extra).

orf_and_gene_to_learn_goal(GeneName,ORF,cluster_hmm(Nucleotides,AnnotList)) :-
        gene(GeneName,GeneLeftEnd,GeneRightEnd,Strand,Frame),
        cluster_gene(ClusterId,GeneName),
        ORF =.. [_,_,OrfLeftEnd,OrfRightEnd,Strand,Frame,ExtraData],
        member(sequence(Nucleotides), ExtraData),
        ORFLength is 1 + OrfRightEnd - OrfLeftEnd,
        GeneLength is 1 + GeneRightEnd - GeneLeftEnd,
        NonCodingLength is ORFLength - GeneLength,
        list_of(0,NonCodingLength,NonCodingList),
        list_of(ClusterId,GeneLength,GeneList),
        append(NonCodingList,GeneList,AnnotList).

non_coding_orf_to_learn_goal(ORF,cluster_hmm(Nucleotides,AnnotList)) :-
        ORF =.. [_,_,_,_,_,_,ExtraData],
        member(sequence(Nucleotides), ExtraData),
        length(Nucleotides,L),
        list_of(0,L,AnnotList).

	

% Create a list of Length each element in which is Item
list_of(Item,Length,[Item|Rest]) :-
        Length > 0,
        NewLength is Length - 1,
        list_of(Item,NewLength,Rest).
list_of(_,0,[]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Deprecated stuff
%
%
%

% Create a details list for all genes with names in the GeneNames list 
/*
select_genes_with_names(GeneNames, Genes, SelectedGenes) :-
        findall(gene(GeneName,LeftEnd,RightEnd,Strand,Frame,Extra),
                (
                        member(GeneNames,GeneName),
                        gene(GeneName,LeftEnd,RightEnd,Strand,Frame,Extra),
                        member(Gene,Genes)
                ), 
                SelectedGenes).
*/


% Given a gene cluster, this predicate unifies NucleotideData to a list of 
% nucleotide sequences (reverse complemented, if necessary) from those genes  
get_cluster_orfs([],_,[]).
get_cluster_orfs([(_,_,GeneNames)|RestClusters],Genes,[(SelectedGenes,ORFs)|RestORFs]) :-
        select_genes_with_names(GeneNames,Genes,SelectedGenes),
        map(get_gene_orf,SelectedGenes,ORFs),
        get_cluster_orfs(RestClusters,Genes,RestORFs).

