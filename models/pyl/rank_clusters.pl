:- ['../../lost.pl'].

:- use(fasta).


	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Rank by size
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
rank_by_size(ClustersFile,ClustersDetailFile,SortedClustersFile,SortedClustersDetailFile) :-
	terms_from_file(ClustersFile,Clusters),
	sort_clusters_by_size(Clusters,SortedClusters),
	terms_to_file(SortedClustersFile,SortedClusters),
	[ClustersDetailFile],
	findall(cluster(Cluster,Details),(member(cluster(Cluster),SortedClusters), cluster_matches(Cluster,Details)),SortedClustersDetail),
	terms_to_file(SortedClustersDetailFile,SortedClustersDetail).

sort_clusters_by_size(Clusters,ClustersBySizeDescending) :-
	add_cluster_size(Clusters,ClustersWithSize),
	sort(ClustersWithSize,ClustersWithSizeBySize),
	add_cluster_size(ClustersBySizeAscending,ClustersWithSizeBySize),
	reverse(ClustersBySizeAscending,ClustersBySizeDescending).

add_cluster_size([],[]).
add_cluster_size([cluster(Cluster)|ClusterRest],[[Size,cluster(Cluster)]|SizeClusterRest]) :-
	length(Cluster,Size),
	writeln(Size),
	add_cluster_size(ClusterRest,SizeClusterRest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Rank by average ORF size in cluster
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rank_by_average_length(ClustersFile,ClustersDetailFile,SortedClustersFile,SortedClustersDetailFile) :-
	terms_from_file(ClustersFile,Clusters),
	sort_clusters_by_average_length(Clusters,SortedClusters),
	terms_to_file(SortedClustersFile,SortedClusters),
	[ClustersDetailFile],
	findall(cluster(Cluster,Details),(member(cluster(Cluster),SortedClusters), cluster_matches(Cluster,Details)),SortedClustersDetail),
	terms_to_file(SortedClustersDetailFile,SortedClustersDetail).

sort_clusters_by_average_length(Clusters,ClustersBySizeDescending) :-
	add_average_cluster_length(Clusters,ClustersWithAvgLength),
	sort(ClustersWithAvgLength,ClustersWithAvgLengthByAvgLength),
	add_average_cluster_length(ClustersByAvgLengthAscending,ClustersWithAvgLengthByAvgLength),
	reverse(ClustersByAvgLengthAscending,ClustersBySizeDescending).
/*n
add_average_cluster_length([],[]).
add_average_cluster_length([Cluster|ClusterRest],[[AvgLength,Cluster]|SizeClusterRest]) :-
	average_cluster_length(Cluster,AvgLength),
	add_average_cluster_length(ClusterRest,SizeClusterRest).
*/

average_cluster_length(cluster(_,Cluster),AvgLength) :-
	average_cluster_length(cluster(Cluster),AvgLength).

average_cluster_length(cluster(Cluster),AvgLength) :-
	write('Average_cluster_length: '),
	findall(L,(member([_,Left,Right|_],Cluster),L is Right-Left),Lengths),
	length(Cluster,NumElems),
	sumlist(Lengths,TotalLengths),
	AvgLength is TotalLengths / NumElems.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Rank by number of organisms
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
rank_by_organisms(ClustersFile,ClustersDetailFile,SortedClustersFile,SortedClustersDetailFile) :-
	terms_from_file(ClustersFile,Clusters),
	sort_clusters_by_number_of_organisms(Clusters,SortedClusters),
	terms_to_file(SortedClustersFile,SortedClusters),
	[ClustersDetailFile],
	findall(cluster(Cluster,Details),(member(cluster(Cluster),SortedClusters), cluster_matches(Cluster,Details)),SortedClustersDetail),
	terms_to_file(SortedClustersDetailFile,SortedClustersDetail).	

sort_clusters_by_number_of_organisms(Clusters,SortedClusters) :-
	add_organisms(Clusters,ClustersA),
	sort(ClustersA,ClustersB),
	add_organisms(ClustersC,ClustersB),
	reverse(ClustersC,SortedClusters).

add_organisms([],[]).
add_organisms([cluster(Cluster)|ClusterRest],[[NumOrganisms,cluster(Cluster)]|OrgClusterRest]) :-
	findall(Organism,member([Organism|_],Cluster),OrganismsDup),
	eliminate_duplicate(OrganismsDup,Organisms),
	length(Organisms,NumOrganisms),
	writeln(NumOrganisms),
	add_organisms(ClusterRest,OrgClusterRest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Rank by diversity measure
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rank_by_diversity(ClustersFile,ClustersDetailFile,SortedClustersFile,F) :-
	add_measures(ClustersFile,ClustersDetailFile,SortedClustersFile),
	atom_concat_list(['touch ',F],Cmd),
	system(Cmd).
	
add_measures(ClustersFile,ClustersDetailFile,SortedClustersFile) :-
	open(ClustersDetailFile,read,Stream),
	clusters_with_pylis_pairs(Stream,Clusters1),
	writeln('aligning sequences'),
	analyze_clusters_with_features(Clusters1,Clusters2),
%	align_sequences(Clusters1,Clusters2),
	add_number_of_organisms(Clusters2,Clusters3),
	add_average_cluster_length(Clusters3,Clusters4),
	normalize_measure(organisms,Clusters4,Clusters5),
	normalize_measure(diversity,Clusters5,Clusters6),
	normalize_measure(orf_length,Clusters6,Clusters7),
	writeln('add_combined_measure'),
	add_combined_measure(Clusters7,Clusters8),
	writeln('Sorting by combined score: '),
	sort(Clusters8,Clusters9),
	reverse(Clusters9,Clusters10),
	writeln('Writing to file: '),
	terms_to_file(SortedClustersFile,Clusters10),
	close(Stream).
	
normalize_measure(M,Clusters,ClustersNorm) :-
	range_for_measure(M,Clusters,9999999,-9999999,Min,Max),
	normalize_measure_rec(M,Min,Max,Clusters,ClustersNorm).

normalize_measure_rec(_,_,_,[],[]).
normalize_measure_rec(M,Min,Max,[cluster(Measures,C)|ClustersRest],[cluster([NewRelMeasure|Measures],C)|ClustersNormRest]) :-
	univ(MatchMeasure,[ M, Value]),
	member(MatchMeasure,Measures),
	RelativeMeasure is (Value-Min) / (Max-Min),
	atom_concat('relative_',M,RelM),
	univ(NewRelMeasure,[ RelM, RelativeMeasure ]),
	normalize_measure_rec(M,Min,Max,ClustersRest,ClustersNormRest).

range_for_measure(_,[],Min,Max,Min,Max).
range_for_measure(M,[cluster(Measures,_)|Cs],Min,Max,FinalMin,FinalMax) :-
	MatchMeasure =.. [ M, Value ],
	member(MatchMeasure,Measures),
	NextMin is min(Min,Value),
	NextMax is max(Max,Value),
	range_for_measure(M,Cs,NextMin,NextMax,FinalMin,FinalMax).

total_for_measure(_,[],0).
total_for_measure(M,[cluster(Measures,_)|Cs],Total) :-
	MatchMeasure =.. [ M, Value ],
	member(MatchMeasure,Measures),
	total_for_measure(Cs,TotalRest),
	Total is Value + TotalRest.
	
add_combined_measure([],[]).
add_combined_measure([cluster(Measures,Cluster)|ClusterRest],[cluster([combined(Combined)|Measures],Cluster)|OrgClusterRest]) :-
	member(relative_diversity(Diversity),Measures),
	member(relative_organisms(Organisms),Measures),
%	member(relative_orf_length(OrfLength),Measures),	
	Combined is Diversity * Organisms,
	add_combined_measure(ClusterRest,OrgClusterRest).

add_number_of_organisms([],[]).
add_number_of_organisms([cluster(Measures,Cluster)|ClusterRest],[cluster([organisms(NumOrganisms)|Measures],Cluster)|OrgClusterRest]) :-
	findall(Organism,member([Organism|_],Cluster),OrganismsDup),
	eliminate_duplicate(OrganismsDup,Organisms),
	length(Organisms,NumOrganisms),
	add_number_of_organisms(ClusterRest,OrgClusterRest).
	
add_average_cluster_length([],[]).
add_average_cluster_length([cluster(Measures,Cluster)|ClusterRest],[cluster([orf_length(AvgLength)|Measures],Cluster)|OrgClusterRest]) :-
%	average_cluster_length(cluster(Cluster),AvgLength),
	findall(L,(member([_,Left,Right|_],Cluster),L is 1+Right-Left),Lengths),
	length(Cluster,NumElems),
	sumlist(Lengths,TotalLengths),
	AvgLength is TotalLengths / NumElems,!,
	add_average_cluster_length(ClusterRest,OrgClusterRest).
	
sort_cluster_by_score(Clusters,ClustersSorted) :-
	findall(ByScore,(member(cluster([O,L,R,S,F,UAG,Score]),Clusters),ByScore=[Score,[O,L,R,S,F,UAG,Score]]),ClustersByScores),
	sort(ClustersByScores,ClusterByScoresSorted),
	findall(Cluster,(member([Score,[O,L,R,S,F,UAG,Score]],ClusterByScoresSorted),Cluster=cluster([O,L,R,S,F,UAG,Score])),ClustersSorted).
	
clusters_with_features(Stream1,[[Cluster,PylisPairs,Scores]|Rest]) :-
	write('.'),
	once(read(Stream1,cluster_matches(Cluster,Matches))),
	pylis_sequences_from_matches(Matches,PylisPairs),
	scores_from_matches(Matches,ScorePairs),
	flatten(ScorePairs,ScoresDup),
	eliminate_duplicate(ScoresDup,Scores),
	!,
	clusters_with_pylis_pairs(Stream1,Rest).

clusters_with_pylis_pairs(Stream1,[[Cluster,PylisPairs]|Rest]) :-
	write('.'),
	once(read(Stream1,cluster_matches(Cluster,Matches))),
	pylis_sequences_from_matches(Matches,PylisPairs),
	!,
	clusters_with_pylis_pairs(Stream1,Rest).
	
clusters_with_pylis_pairs(_,[]).
		
pylis_sequences_from_matches([],[]).
pylis_sequences_from_matches([blast_match(_,_LeftA,_RightA,_StrandA,_FrameA,ExtraMatch)|RestMatches],[[PylSeqA,PylSeqB]|RestSeqs]) :-
	member(match_to(orf(_,_LeftB,_RightB,_StrandB,_FrameB,ExtraB)), ExtraMatch),!,
	member(query_orf(orf(_,_LeftA,_RightA,_StrandA,_FrameA,ExtraA)), ExtraMatch),!,
	member(pylis_sequence(PylSeqA),ExtraA),!,
	member(pylis_sequence(PylSeqB),ExtraB),!,
	pylis_sequences_from_matches(RestMatches,RestSeqs).
	
scores_from_matches([],[]).
scores_from_matches([blast_match(_,_LeftA,_RightA,_StrandA,_FrameA,ExtraMatch)|RestMatches],[[ScoreA,ScoreB]|RestScores]) :-
	member(match_to(orf(_,_LeftB,_RightB,_StrandB,_FrameB,ExtraB)), ExtraMatch),!,
	member(query_orf(orf(_,_LeftA,_RightA,_StrandA,_FrameA,ExtraA)), ExtraMatch),!,
	member(codon_score(ScoreA),ExtraA),!,
	member(codon_score(ScoreB),ExtraB),!,
	scores_from_matches(RestMatches,RestScores).
	
analyze_clusters_with_features([[Cluster,PylisPairs,Scores]|Rest],[cluster([diversity(AlignmentScore),codon_score(CodonScore)],Cluster)|RestScored]) :-
	% Calculate alignment score:
	align_pairs(PylisPairs,AlignScores),
	length(AlignScores,NumAlignScores),
	sumlist(AlignScores,AlignTotal),
	AlignmentScore is AlignTotal / NumAlignScores,
	% Calculate average codon score
	length(Scores,NumCodonScores),
	sumlist(Scores,CodonTotal),
	CodonScore is CodonTotal / NumCodonScores,
	analyze_clusters_with_features(Rest,RestScored).

/*
align_sequences([],[]).

align_sequences([[Cluster,PylisPairs]|Rest],[cluster([diversity(AverageScore)],Cluster)|RestScored]) :-
	write('.'),
	align_pairs(PylisPairs,Scores),
	length(Scores,NumScores),
	sumlist(Scores,Total),
	AverageScore is Total / NumScores,
	align_sequences(Rest,RestScored).
*/

align_pairs([],[]).
align_pairs([[Seq1,Seq2]|SeqRest],[Score|ScoresRest]) :-
	edit(Seq1,Seq2,Score),
	align_pairs(SeqRest,ScoresRest).

:- table edit/3.

edit([],[],0).
edit([],[_Y|Ys],Dist) :- edit([],Ys,Dist1), Dist is 1 + Dist1.
edit([_X|Xs],[],Dist) :- edit(Xs,[],Dist1), Dist is 1 + Dist1.

edit([X|Xs],[Y|Ys],Dist) :-
	edit([X|Xs],Ys,InsDist),
	edit(Xs,[Y|Ys],DelDist),
	edit(Xs,Ys,TailDist),
	(X==Y -> 
		% Match
		Dist = TailDist
		; 
		InsDist1 is InsDist + 1,
		DelDist1 is DelDist + 1,
		TailDist1 is TailDist + 1,
		% minimal of insertion, deletion or substitution
		sort([InsDist1,DelDist1,TailDist1],[Dist|_])).
		
write_fasta_file(Filename,SequencePairs) :-
	writeln(write_fasta_file(Filename)),
	findall(Sequence,member([Sequence,_],SequencePairs),Sequences1),
	findall(Sequence,member([_,Sequence],SequencePairs),Sequences2),
	append(Sequences1,Sequences2,Sequences),
	eliminate_duplicate(Sequences,UniqSequences),
	findall(Fasta,
			(nth1(N,UniqSequences,Sequence),
			number_codes(N,NC),
			atom_codes(NA,NC),
			to_fasta(NA,Sequence,Fasta)),
			FastaCodes),
	flatten(FastaCodes,FastaCodesFlat),
	open(Filename,write,Stream),
	forall(member(Code,FastaCodesFlat),put_code(Stream,Code)),
	close(Stream).

test :- 
	add_measures('clusters100.pl','cluster_matches100.pl','clusters_measures.pl').
	
test1 :- 
	rank_by_average_length('clusters100.pl','cluster_matches100.pl','clusters_sorted.pl','clusters_detail_sorted.pl').	

test2 :- 
	rank_by_size('clusters.pl','cluster_matches.pl','clusters_sorted.pl','clusters_detail_sorted.pl').
	
test3 :-
	rank_by_organisms('clusters.pl','cluster_matches.pl','clusters_sorted.pl','clusters_detail_sorted.pl').