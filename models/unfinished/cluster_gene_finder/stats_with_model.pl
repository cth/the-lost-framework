% Generate statistics using the actual PRISM model
% 
% Need: Gene + nucleotide data
% Chew through orfs..

:- ['../../lost.pl'].
:- lost_include_api(sequence).
:- lost_include_api(io).
:- lost_include_api(misc_utils).
:- lost_include_api(viterbi_learn).

create_stats_with_model(GenesFile,GenomeFile,StatsCSVFile,StatsLabelsFile)  :-
       assert(coding_clusters(1)),
       create_stat_labels_file(clustgf, StatsLabelsFile),
       create_stats_from_model(clustgf,GenesFile,GenomeFile,StatsCSVFile),
       retract(coding_clusters(1)).

create_stat_labels_file(Model,File) :-
        prismAnnot(Model,direct),
        set_sw_all,
        all_switch_instances(Instances),
        extract_label_parts(Instances,Labels),
        intersperse('\n',[gene|Labels],LabelsInterspersed),
        tell(File),
        forall(member(T,LabelsInterspersed), write(T)),
        told.

% Skip if stats could not be correctly calculated (indicated by nil value)
write_stats_as_csv(_,stats(_,nil)).

write_stats_as_csv(Stream,stats(GeneName,MSWList)) :-
        extract_stat_parts(MSWList,StatsList),
        intersperse(',',[GeneName|StatsList], CSVList),
        forall(member(T,CSVList), write(Stream,T)),
        write(Stream,'\n').

extract_label_parts([],[]).
extract_label_parts([sw(SwitchName,Outcome,_)|As], [sw(SwitchName,Outcome)|Bs]) :-
        extract_label_parts(As,Bs).

extract_stat_parts([],[]).
extract_stat_parts([sw(_,_,P)|As],[P|Bs]) :-
        extract_stat_parts(As,Bs).

create_stats_from_model(Model,GenesFile,GenomeFile,StatsFile) :-
        prismAnnot(Model,direct),
        write('loading genome..'),
	load_sequence(genome,GenomeFile),
        write(done),nl,
        !,
        write('loading genes..'),
	terms_from_file(GenesFile, Genes),
        write('done'),nl,
        !,
        open(StatsFile,write,Stream),
        !,
        forall( member(Gene,Genes), (
                (create_gene_statistics(Gene,Stats),!,
                write_stats_as_csv(Stream,Stats)) 
                )
        ),
        close(Stream).

%create_and_write_gene_stats_as_csv([],Stream).
%create_and_write_gene_stats_as_csv([Gene|Rs],Stream) :-
%      create_gene_statistics(Gene,Stats),!,
%      write_stats_as_csv(Stream,Stats),
%      !,
%      create_and_write_gene_stats_as_csv(Rs,Stream).

create_gene_model_goal(Gene,cluster_hmm(Nucleotides,Annot)) :-
        write('get_sequence'),nl,
	get_gene_nucleotide_sequence(Gene,Nucleotides),
        write('done'),nl,
	length(Nucleotides, GeneLength),
	list_of(1,GeneLength, Annot).

create_gene_statistics(Gene,stats(Name,Stats)) :-
        write('.'),
        gene_name(Gene,Name),
        create_gene_model_goal(Gene,Goal),
	clear_counters,
        (viterbi_learn_term(Goal) ->
        	add_pseudo_counts,
        	set_switches_from_counts,
                table_remove(_),
                all_switch_instances(SwitchInstances),
                sort(SwitchInstances,Stats)
                ;
                write('could not learn goal:'),
                %write(Goal),
                Stats = nil
        ).

gene_name(Gene,Name) :-
        Gene =.. [ _functor, _LeftEnd, _RightEnd, _strand, _frame, Extra ],
        member(gene_name(Name),Extra). 

        
% Make a list of all switches complete with their outcomes
% and the probability of those outcomes 
all_switch_instances(SwitchInstances) :-
        findall(sw(SwName,SwOutcome,SwProb), 
                (get_sw(switch(SwName,_,SwOutcomes,SwProbs)),
                nth(N,SwOutcomes,SwOutcome), 
                nth(N,SwProbs,SwProb)),
                SwitchInstances).
                 
scale_by(_,[],[]).
scale_by(ScaleBy,[Unscaled|Us],[Scaled|Ss]) :-
	Scaled is Unscaled * ScaleBy,
	!,
	scale_by(ScaleBy,Us,Ss).

% Scales each probabilities according to the number of outcomes for particular msw
scaled_switch(SwitchName,Outcomes,ScaledOutcomeProbs) :-
	get_sw(switch(SwitchName, _fixed, Outcomes, OutcomeProbs)),
	length(Outcomes,NumOutcomes),
	scale_by(NumOutcomes,OutcomeProbs,ScaledOutcomeProbs).

	
% Create a list of Length each element in which is Item
list_of(Item,Length,[Item|Rest]) :-
        Length > 0,
        NewLength is Length - 1,
        list_of(Item,NewLength,Rest).
list_of(_,0,[]).

get_gene_nucleotide_sequence(Gene, Nucleotides) :-
        Gene =.. [ _functor, LeftEnd, RightEnd, Strand, _frame, _extra ],
        get_sequence_range(genome,LeftEnd,RightEnd,Sequence),
        ((Strand = '+') ->
                Nucleotides = Sequence
                ;
                dna_seq_complement(Sequence,SequenceComplemented),
                reverse(SequenceComplemented,Nucleotides)
        ).

/*
 * Stuff below here is not used
 */
get_noncoding_nucleotide_sequence_before_gene(Genes,Gene,Nucleotides) :-
        Gene =.. [ _functor, LeftEnd, RightEnd, '+', _frame, _extra ],
	((Strand == '+') -> RangeRightEnd is LeftEnd - 1 ; RangeLeftEnd is RightEnd + 1),
	% If the is a previous (in reading direction) gene in the same reading frame
	% then we are interested in the range between the two genes
        (previous_gene_same_frame(Genes,Gene,PreviousGene) ->
	 PreviousGene =.. [ _, LeftEnd,RightEnd,_,_,_],
	 ((Strand == '+') -> RangeLeftEnd is RightEnd + 1 ; RangeRightEnd is LeftEnd - 1)
	 ;
	 ((Strand == '+') -> RangeLeftEnd = 1 ; RangeRightEnd = max)
	 ),
        get_sequence_range(genome,RangeLeftEnd,RangeRightEnd,Sequence),
        ((Strand = '+') ->
                Nucleotides = Sequence
                ;
                dna_seq_complement(Sequence,SequenceComplemented),
                reverse(SequenceComplemented,Nucleotides)
        ).

previous_gene_same_frame(Genes,Gene,PreviousGene) :-
        Gene =.. [ _, LeftEnd, _, '+', Frame, _ ],
        findall(RightEnd1,
		(member(G1,Genes),G1 =.. [_,_,RightEnd1,'+',Frame,_]),
		AllGeneEnds),
	PreviousGeneEnd :: AllGeneEnds,
	PreviousGeneEnd #< LeftEnd,
	maxof(labeling(PreviousGeneEnd), PreviousGeneEnd),
	member(PreviousGene,Genes),
	PreviousGene =.. [ _, _, PreviousGeneEnd, '+',Frame,_].


previous_gene_same_frame(Genes,Gene,PreviousGene) :-
        Gene =.. [ _, _, RightEnd, '-', Frame, _ ],
        findall(LeftEnd1,
		(member(G1,Genes),G1=..[_,LeftEnd1,_,'-',Frame,_]),
		AllGeneEnds),
	PreviousGeneEnd :: AllGeneEnds,
	PreviousGeneEnd #> RightEnd,
	minof(labeling(PreviousGeneEnd), PreviousGeneEnd),
	member(PreviousGene,Genes),
	PreviousGene =.. [_,PreviousGeneEnd,_,'-',Frame,_].