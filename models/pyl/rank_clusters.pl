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

add_average_cluster_length([],[]).
add_average_cluster_length([Cluster|ClusterRest],[[AvgLength,Cluster]|SizeClusterRest]) :-
	average_cluster_length(Cluster,AvgLength),
	add_average_cluster_length(ClusterRest,SizeClusterRest).

average_cluster_length(cluster(Cluster),AvgLength) :-
	write('Average_cluster_length: '),
	findall(L,(member([_,Left,Right|_],Cluster),L is Right-Left),Lengths),
	length(Cluster,NumElems),
	sumlist(Lengths,TotalLengths),
	AvgLength is TotalLengths / NumElems,
	writeln(AvgLength).


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
	clusters_with_pylis_pairs(Stream,ClustersWithPairs),
	nl,
	writeln('Aligning sequences:'),
	align_sequences(ClustersWithPairs,ClustersWithScores),
	writeln('add_number_of_organisms'),
	add_number_of_organisms(ClustersWithScores,ClustersWithOrganisms),
	writeln('add_combined_measure'),
	add_combined_measure(ClustersWithOrganisms,ClustersWithCombined),
	writeln('Sorting by combined score: '),
	sort(ClustersWithCombined,ClustersByScores),
	reverse(ClustersByScores,ClustersByScoresRev),
	writeln('Writing to file: '),
	terms_to_file(SortedClustersFile,ClustersByScoresRev),
	close(Stream).
	
add_combined_measure([],[]).
add_combined_measure([cluster(Measures,Cluster)|ClusterRest],[cluster([combined(Combined)|Measures],Cluster)|OrgClusterRest]) :-
	member(diversity(Diversity),Measures),
	member(organisms(Organisms),Measures),
	Combined is Diversity * Organisms,
	add_combined_measure(ClusterRest,OrgClusterRest).

add_number_of_organisms([],[]).
add_number_of_organisms([cluster(Measures,Cluster)|ClusterRest],[cluster([organisms(NumOrganisms)|Measures],Cluster)|OrgClusterRest]) :-
	findall(Organism,member([Organism|_],Cluster),OrganismsDup),
	eliminate_duplicate(OrganismsDup,Organisms),
	length(Organisms,NumOrganisms),
	add_number_of_organisms(ClusterRest,OrgClusterRest).

sort_cluster_by_score(Clusters,ClustersSorted) :-
	findall(ByScore,(member(cluster([O,L,R,S,F,UAG,Score]),Clusters),ByScore=[Score,[O,L,R,S,F,UAG,Score]]),ClustersByScores),
	sort(ClustersByScores,ClusterByScoresSorted),
	findall(Cluster,(member([Score,[O,L,R,S,F,UAG,Score]],ClusterByScoresSorted),Cluster=cluster([O,L,R,S,F,UAG,Score])),ClustersSorted).

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
		
align_sequences([],[]).

align_sequences([[Cluster,PylisPairs]|Rest],[cluster([diversity(AverageScore)],Cluster)|RestScored]) :-
	write('.'),
	align_pairs(PylisPairs,Scores),
	length(Scores,NumScores),
	sumlist(Scores,Total),
	AverageScore is Total / NumScores,
	align_sequences(Rest,RestScored).

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