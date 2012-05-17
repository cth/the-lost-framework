:- [annotated_pylis].
:- ['pyl_add_cluster_features_3015_1.gen'].
%:- ['rank_by_organisms.pl'].

go :-
	findall(annotated(Organism,Left,Right,Strand,Frame,Extra),gb(Organism,Left,Right,Strand,Frame,Extra),AnnotatedGenes),
	in_clusters(AnnotatedGenes,InClusters,OutClusters,RelevantClusters),
	writeln('Annotated genes present in clusters:'),
	by_cluster(InClusters,RelevantClusters,[],ByClusters),
	forall(member([C,Genes],ByClusters),
		(write('Cluster '), write(C), writeln(': '), writelist(Genes))),
	writeln('Annotated genes not present in clusters:'),
	writelist(OutClusters),
	eliminate_duplicate(RelevantClusters,UniqClusters),
	report_cluster_features(UniqClusters),
	writeln('Ranking: '),
	cluster_hashcodes(UniqClusters,HashCodes),
	write_rank_for_file(HashCodes,'rank_by_organisms.pl'),
	write_rank_for_file(HashCodes,'rank_by_diversity.pl'),
	write_rank_for_file(HashCodes,'rank_by_codon_score.pl'),
	write_rank_for_file(HashCodes,'rank_by_pmcomp.pl'),
	write_rank_for_file(HashCodes,'rank_by_orf_length.pl'),
	write_rank_for_file(HashCodes,'rank_by_syn_codons.pl'),
	write_rank_for_file(HashCodes,'rank_by_avg_upstream_uag.pl'),
	write_rank_for_file(HashCodes,'rank_by_codon_and_upstream.pl'),
	write_rank_for_file(HashCodes,'rank_by_pmcomp_and_diversity.pl'),
	write_rank_for_file(HashCodes,'rank_by_combined.pl'),
	nl,writeln('--- Linear regression ranking: ---'),nl,
	load_clauses('rank_by_organisms.pl',AllClusters),
	assert(best_ranks([1000000])),
	assert(best_weights(na)),
	!,
	linear_regression_ranking(AllClusters,HashCodes).
	
	
in_clusters([],[],[],[]).
	
in_clusters([Gene|GeneRest],[Gene|InClusterRest],NotInClusterRest,[cluster(Features,Members)|ClustersRest]) :-
	Gene = annotated(GeneOrganism,_,_,_,_,Extra),
	spaces_to_underscores(GeneOrganism,Organism),
	member(in_frame_stops([UAG]),Extra),
	cluster(Features,Members),
	member([Organism,_left,_right,_strand,_frame,UAG],Members),
	in_clusters(GeneRest,InClusterRest,NotInClusterRest,ClustersRest).

in_clusters([Gene|GeneRest],InClusterRest,[Gene|NotInClusterRest],ClustersRest) :-
	in_clusters(GeneRest,InClusterRest,NotInClusterRest,ClustersRest).
	
writelist([]) :- nl.
writelist([annotated(Organism,Left,Right,Strand,Frame,Extra)|Xs]) :-
	member(product(Prod),Extra),
	member(in_frame_stops([UAG]),Extra),
	writeln([Organism,Left,Right,Strand,Frame,uag(UAG),Prod]),
	writelist(Xs).

by_cluster([],[],InOut,InOut).

by_cluster([X|Xs],[cluster(Features,_)|Cs],OrderedIn,OrderedOut) :-
	hash_code(Features,C),
	member([C,Genes],OrderedIn),!,
	subtract(OrderedIn,[[C,Genes]],RestOrdered),
	by_cluster(Xs,Cs,[[C,[X|Genes]]|RestOrdered],OrderedOut).

by_cluster([X|Xs],[cluster(Features,_)|Cs],OrderedIn,OrderedOut) :-
	hash_code(Features,C),
	by_cluster(Xs,Cs,[[C,[X]]|OrderedIn],OrderedOut).

spaces_to_underscores(A,B) :-
	atom_codes(A,CodesA),
	spaces_to_underscores_list(CodesA,CodesB),
	atom_codes(B,CodesB).
	
spaces_to_underscores_list([],[]).
spaces_to_underscores_list([32|Xs],[95|Ys]) :-
	!,
	spaces_to_underscores_list(Xs,Ys).
spaces_to_underscores_list([X|Xs],[X|Ys]) :-
	spaces_to_underscores_list(Xs,Ys).
	
report_cluster_features(Clusters) :-
	Features = [ organisms, size, orf_length, diversity, codon_score, pmcomp,avg_upstream_uag,avg_downstream_uag,diversity_ratio,syn_codons,hash_code],
	forall(member(F,Features),(writeln(F),report_measure(F,Clusters),nl)).
	

cluster_hashcodes([],[]).

cluster_hashcodes([cluster(Features,_)|Cs],[HashCode|RestHash]) :-
	hash_code(Features,HashCode),
	cluster_hashcodes(Cs,RestHash).

report_measure(_Measure,[]).

report_measure(hash_code,[cluster(Features,_)|Cs]) :-	
	hash_code(Features,HashCode),
	write(HashCode),
	write(','),
	report_measure(hash_code,Cs).

report_measure(Measure,[cluster(Features,_)|Cs]) :-
	Measure \= hash_code,
	Matcher =.. [ Measure, Value ],
	member(Matcher,Features),
	write(Value),
	write(','),
	report_measure(Measure,Cs).
	
write_rank_for_file(HashCodes,File) :-
	load_clauses(File,Clauses),
	calc_rank(Clauses,HashCodes,Ranks,AvgRank,RelRank),
	writeln(File),
	write(HashCodes),
	write('\t\t'),
	write(Ranks),	
	write('\t\t'),
	write(AvgRank),
	write('\t\t'),
	write(RelRank),
	nl.
		
calc_rank(Clusters,HashCodes,Ranks,AvgRank,RelRank) :-
	findall(Rank,(member(HashCode,HashCodes),rank_for_cluster(HashCode,Clusters,Rank)),Ranks),
	sumlist(Ranks,Total),
	length(Ranks,NumRanks),
	AvgRank is Total / NumRanks,
	RelRank is 1 / AvgRank.

rank_for_cluster(HashCode,Clusters,Rank) :-
	member(C1,Clusters),
	C1=cluster(Features,_),
	hash_code(Features,HashCode),
	nth1(Rank,Clusters,C1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% linear_regression_ranking
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

random_weights(0,[]).
random_weights(N,[X|Xs]) :-
	random_gaussian(X),
	N1 is N - 1,
	random_weights(N1,Xs).
/*
	InitialWeights = [
%		relative_organisms(0.009),
		relative_organisms(X1),
%		relative_diversity(0.003),
%		relative_pmcomp(0.002),	
		relative_codon_score(0.022),
		relative_avg_upstream_uag(0.041),
		codon_and_upstream(0.083),
		pmcomp_and_diversity(0.003)
%		relative_diversity_ratio(0.001),
%		relative_syn_codons(0.5),
%		relative_orf_length(0.5)
	 ],
*/

linear_regression_ranking(Clusters,KnownClusterHashCodes) :-
	extract_clusters(Clusters,KnownClusterHashCodes,KnownClusters),
	random_weights(7,[X1,X2,X3,X4,X5,X6,X7]),
	InitialWeights = [
		relative_organisms(X1),
		relative_codon_score(X2),
		relative_avg_upstream_uag(X3),
		relative_diversity(X4),
		relative_pmcomp(X5),
		codon_and_upstream(-0.108471930074946),
		pmcomp_and_diversity(0.000114833815124)
	 ],
	linear_regression_ranking_rec(100000,0.1,InitialWeights,FinalWeights,Clusters,KnownClusters),
	writeln(final_weights(FinalWeights)).
	
linear_regression_ranking_rec(0,_,WeightsInOut,WeightsInOut,_,_).

linear_regression_ranking_rec(N,LearningRate,WeightsIn,WeightsOut,Clusters,KnownClusters) :-
	N > 0,
	cluster_hashcodes(KnownClusters,KnownCodes),
	calculate_ranks(Clusters,WeightsIn,KnownCodes,Ranks),
	update_weights(WeightsIn,LearningRate,Ranks,KnownClusters,WeightsNext),
%	writeln(weights(WeightsNext)),
	N1 is N - 1,
	writeln(learning_rate(LearningRate)),
%	NextLearningRate is LearningRate - (LearningRate * LearningRate * LearningRate),
	NextLearningRate is LearningRate,
	linear_regression_ranking_rec(N1,NextLearningRate,WeightsNext,WeightsOut,Clusters,KnownClusters).
		
extract_clusters(_,[],[]).
extract_clusters(Clusters,[H|Hs],[C|Cs]) :-
	member(C,Clusters),
	C = cluster(Features,_),
	hash_code(Features,H),!,
	extract_clusters(Clusters,Hs,Cs).

/*
update_weights(Weights,LearningRate,PredictedRanks,Clusters,UpdatedWeights) :-
	best_ranks(BestRanks),
	sumlist(PredictedRanks,TotalLoss),
	sumlist(BestRanks,BestLoss),
	((TotalLoss < BestLoss) ->
		retract(best_ranks(BestRanks)),
		retract(best_weights(BestWeights)),
		assert(best_ranks(PredictedRanks)),
		assert(best_weights(Weights)),
		writeln('new best: '),
		write(ranks(PredictedRanks)),
		write('total_loss: '), writeln(TotalLoss),
		writeln(weights(Weights))
		;
		true
	),
	findall(UpdatedWeight,(member(Weight,Weights),update_single_weight(Weight,LearningRate,PredictedRanks,Clusters,UpdatedWeight)), UpdatedWeightsRaw),
	normalize_weights(UpdatedWeightsRaw,UpdatedWeights).
*/
update_weights(Weights,LearningRate,PredictedRanks,Clusters,UpdatedWeights) :-
	sumlist(PredictedRanks,TotalLoss),
	write(ranks(PredictedRanks)),
	write(' total_loss: '), writeln(TotalLoss),
	writeln(weights(Weights)),
	findall(UpdatedWeight,(member(Weight,Weights),update_single_weight(Weight,LearningRate,PredictedRanks,Clusters,UpdatedWeight)), UpdatedWeights).
%	normalize_weights(UpdatedWeightsRaw,UpdatedWeights).


update_single_weight(Weight,LearningRate,PredictedRanks,Clusters,UpdatedWeight) :-
	length(Clusters,M),
	Weight =.. [ Functor, WeightValue ],
	findall(V,(member(C,Clusters),feature_value(Functor,C,V)),FeatureValues),
	multiply_lists(PredictedRanks,FeatureValues,Products),
	sumlist(Products,InnerSum1),
	InnerSum is 1-(1/(InnerSum1 - 21)),
%	UpdatedWeightValue is WeightValue - (LearningRate * (InnerSum / M)),%
	UpdatedWeightValue is WeightValue - (LearningRate * (InnerSum / (5*M))),
	UpdatedWeight =.. [ Functor, UpdatedWeightValue ].


normalize_weights(RawWeights,NormWeights) :-
	findall(Value,(member(Weight,RawWeights),univ(Weight,[_functor,Value])),WeightValues),
	sort(WeightValues,SortedWeights),
	reverse(SortedWeights,SortedWeightsRev),
	SortedWeights = [Min|_],
	SortedWeightsRev = [Max|_],
	Range is Max - Min,
	((Min < 0) ->
		findall(PosValue,(member(WV,WeightValues),PosValue is -1*Min + WV), PositiveWeightValues)
		;
		PositiveWeightValues = WeightValues),
	findall(NormValue,(member(P,PositiveWeightValues),NormValue is P / Range), NormWeights).
	

	
feature_value(Feature,cluster(Features,_),Value) :-
	FeatureMatcher =.. [ Feature, Value ],
	member(FeatureMatcher,Features).

multiply_lists([],[],[]).
multiply_lists([X|Xs],[Y|Ys],[Z|Zs]) :-
%	Y1 is Y - 0.5,
	Y1 is Y,
	Z is X * Y1,
	multiply_lists(Xs,Ys,Zs).

calculate_ranks(Clusters,Weights,KnownCodes,Ranks) :-
	% Do predictions for each cluster
	findall([Predict,Cluster],(member(Cluster,Clusters),predict_for_cluster(Weights,Cluster,Predict)), MeasuredClusters),
	sort(MeasuredClusters,SortedMeasuredClusters),
%	reverse(SortedMeasuredClustersRev,SortedMeasuredClusters),
	findall(Cluster,member([_,Cluster],SortedMeasuredClusters),RankedClusters),
	findall(Rank,(member(HashCode,KnownCodes),rank_for_cluster(HashCode,RankedClusters,Rank)),Ranks).
%	sumlist(Ranks,Loss). % FIXME: modify to have loss=0 for perfect ranking.

predict_for_cluster(Weights, Cluster, Predict) :-
	Cluster = cluster(Features,_),
	findall(Value,(member(Weight,Weights),apply_weight(Weight,Features,Value)),Values),
	sumlist(Values,Predict).
	
apply_weight(Weight,Features,Value) :-
	Weight =.. [ Functor, WeightValue ],
	FeatureMatcher =.. [ Functor, FeatureValue ],
	member(FeatureMatcher,Features),
	Value is WeightValue * FeatureValue.
