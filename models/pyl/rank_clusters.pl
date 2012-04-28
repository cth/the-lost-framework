:- ['../../lost.pl'].

:- use(fasta).
:- use(dnaseq).
:- [pmcomp].

rank_by_feature(Feature,ClustersWithFeaturesFile,RankedClustersFile) :-
        terms_from_file(ClustersWithFeaturesFile,ClustersWithFeatures),
        %add_measures(ClustersDetailFile,ClustersWithFeatures),!,
        add_feature_key(Feature,ClustersWithFeatures,ClustersByFeature),!,
        sort(ClustersByFeature,SortedClustersFeature),!,
        add_feature_key(Feature,SortedClusters,SortedClustersFeature),!,
        reverse(SortedClusters,RankedClusters),!,
        terms_to_file(RankedClustersFile,RankedClusters).

add_features(ClustersDetailFile,ClustersWithFeaturesFile) :-
        add_measures(ClustersDetailFile,ClustersWithFeatures),
        terms_to_file(ClustersWithFeaturesFile,ClustersWithFeatures).


add_feature_key(_FeatureKey,[],[]).
add_feature_key(FeatureKey,[cluster(Features,Members)|Cs],[[Value,cluster(Features,Members)]|Fs]) :-
        FeatureMatch =.. [ FeatureKey, Value ],
        member(FeatureMatch,Features),
        add_feature_key(FeatureKey,Cs,Fs).
	
add_measures(ClustersDetailFile,ClustersWithFeatures) :-
	open(ClustersDetailFile,read,Stream),
        writeln('open clusters detail file'),
	clusters_with_features(Stream,Clusters1),!,
        nl,
	writeln('aligning sequences'),
	analyze_clusters_with_features(Clusters1,Clusters2),!,
        writeln('analyzed clusters with features'),
	add_number_of_organisms(Clusters2,Clusters3),
        writeln('added number of organisms'),
	add_average_cluster_length(Clusters3,Clusters4),
        writeln('added average cluster length'),
        add_bases_upstream(Clusters4,Clusters5),
	normalize_measure(organisms,Clusters5,Clusters6),
        writeln('added normalized organisms'),
	normalize_measure(diversity,Clusters6,Clusters7),
        writeln('added normalized diversity'),
	normalize_measure(orf_length,Clusters7,Clusters8),
        writeln('added normalized orf length'),
        normalize_measure(codon_score,Clusters8,Clusters9),
        writeln('added normalized codon score'),
        normalize_measure(avg_upstream_uag,Clusters9,Clusters10),
        writeln('added normalized avg_upstream_uag score'),
        normalize_measure(syn_codons,Clusters10,Clusters11),
        normalize_measure(pmcomp,Clusters11,Clusters12),
        normalize_measure(diversity_ratio,Clusters12,Clusters13),
        add_codon_and_upstream(Clusters13,Clusters14),
        writeln('added codon+upstream'),
	add_combined_measure(Clusters14,Clusters15),
	writeln('added_combined_measure'),
        ClustersWithFeatures = Clusters15,
	close(Stream).
	
normalize_measure(M,Clusters,ClustersNorm) :-
	range_for_measure(M,Clusters,9999999,-9999999,Min,Max),
	normalize_measure_rec(M,Min,Max,Clusters,ClustersNorm).

normalize_measure_rec(_,_,_,[],[]).
normalize_measure_rec(M,Min,Max,[cluster(Measures,C)|ClustersRest],[cluster([NewRelMeasure|Measures],C)|ClustersNormRest]) :-
	univ(MatchMeasure,[ M, Value]),
	member(MatchMeasure,Measures),
	RelativeMeasure is ((Value-Min)+1) / ((Max-Min)+1),
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
	
add_codon_and_upstream([],[]).
add_codon_and_upstream([cluster(Measures,Cluster)|ClusterRest],[cluster([codon_and_upstream(Combined)|Measures],Cluster)|OrgClusterRest]) :-
        member(relative_avg_upstream_uag(UAGUP),Measures),
        member(relative_codon_score(Score),Measures),
	Combined is sqrt(UAGUP * Score),
	add_codon_and_upstream(ClusterRest,OrgClusterRest).

add_combined_measure([],[]).
add_combined_measure([cluster(Measures,Cluster)|ClusterRest],[cluster([combined(Combined)|Measures],Cluster)|OrgClusterRest]) :-
        OrganismsWeight = 0.5,
        OrfLengthWeight = 0.044,
        DiversityWeight = 0.008, 
        UpstreamUAGWeight = 0.25,
        CodonScoreWeight = 0.055,
        CodonAndUpstreamWeight = 0.5, 
	member(relative_organisms(Organisms),Measures),
        member(relative_orf_length(OrfLength),Measures),
        member(relative_diversity(Diversity),Measures),
        member(relative_avg_upstream_uag(UAGUP),Measures),
        member(relative_codon_score(CodonScore),Measures),
        member(codon_and_upstream(CodonAndUpstream),Measures),
/*	Combined is 1 /  exp( -1 * (Organisms *  OrganismsWeight +
                                    OrfLength * OrfLengthWeight + 
                                    Diversity * DiversityWeight +  
                                    UAGUP *  UpstreamUAGWeight +
                                    CodonScore * CodonScoreWeight +
                                    CodonAndUpstream * CodonAndUpstreamWeight)),
*/
        Combined is Organisms *  OrganismsWeight +
                 OrfLength * OrfLengthWeight + 
                 Diversity * DiversityWeight +  
                 UAGUP *  UpstreamUAGWeight +
                 CodonScore * CodonScoreWeight +
                 CodonAndUpstream * CodonAndUpstreamWeight,
	add_combined_measure(ClusterRest,OrgClusterRest).

add_bases_upstream([],[]).
add_bases_upstream([cluster(Measures,Cluster)|ClusterRest],[cluster([avg_upstream_uag(AvgBasesUpstream)|Measures],Cluster)|OrgClusterRest]) :-
        findall(BasesUpstream,(member(C,Cluster), bases_upstream(C,BasesUpstream)), AllBasesUpstream),
        length(AllBasesUpstream,NumElems),
        sumlist(AllBasesUpstream,TotalBasesUpstream),
        AvgBasesUpstream is TotalBasesUpstream / NumElems,!,
        add_bases_upstream(ClusterRest,OrgClusterRest).

bases_upstream([_,_Left,Right,-,_,UAG],Bases) :-
        Bases is Right - UAG - 2.
bases_upstream([_,Left,_Right,+,_,UAG],Bases) :-
        Bases is UAG - Left.

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
	write('+'),
	once(read(Stream1,cluster_matches(Cluster,Matches))),
	pylis_sequences_from_matches(Matches,PylisPairs),
	scores_from_matches(Matches,ScorePairs),
	flatten(ScorePairs,ScoresDup),
	eliminate_duplicate(ScoresDup,Scores),
	!,
	clusters_with_features(Stream1,Rest).

clusters_with_features(_,[]).

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
	
analyze_clusters_with_features([],[]).
analyze_clusters_with_features([[Cluster,PylisPairs,Scores]|Rest],[cluster([diversity_ratio(RatioScore),pmcomp(PMAlignmentScore),syn_codons(ProtAlignmentScore),diversity(AlignmentScore),codon_score(CodonScore)],Cluster)|RestScored]) :-
        write('+'),
	% Calculate alignment score:
        %writeln(analyze(Cluster)),
        %writeln(scores(Scores)),
	align_pairs(PylisPairs,AlignScores),
	length(AlignScores,NumAlignScores),
	sumlist(AlignScores,AlignTotal),
	AlignmentScore is AlignTotal / NumAlignScores,

        align_pairs_protein(PylisPairs,ProtAlignScores),
        sumlist(ProtAlignScores,ProtAlignTotal),
        ProtAlignmentScore is ProtAlignTotal / NumAlignScores,

        align_pairs_pmcomp(PylisPairs,PMAlignScores),
        sumlist(PMAlignScores,PMTotal),
        PMAlignmentScore is PMTotal / NumAlignScores,

        align_pairs_ratio(PylisPairs,Ratios),
        sumlist(Ratios,RatiosTotal),
        RatioScore is RatiosTotal / NumAlignScores,

	% Calculate average codon score
	length(Scores,NumCodonScores),
	sumlist(Scores,CodonTotal),
	CodonScore is CodonTotal / NumCodonScores,
	analyze_clusters_with_features(Rest,RestScored).

align_pairs_protein([],[]).
align_pairs_protein([[Seq1,Seq2]|SeqRest],[Score1|ScoresRest]) :-
        write('#'),
        translate(11,Seq1,ProtSeq1),
        translate(11,Seq2,ProtSeq2),
        hamming(ProtSeq1,ProtSeq2,Score),
        ((Score == 0) ->
                Score1 = 0
                ;
                Score1 is 1 / Score),
        align_pairs_protein(SeqRest,ScoresRest).

align_pairs([],[]).
align_pairs([[Seq1,Seq2]|SeqRest],[Score|ScoresRest]) :-
        write('*'),
	edit(Seq1,Seq2,Score),
	align_pairs(SeqRest,ScoresRest).


align_pairs_ratio([],[]).
align_pairs_ratio([[Seq1,Seq2]|SeqRest],[Ratio|RatiosRest]) :-
        split_seq(Seq1,Seq1First,Seq1Last),
        split_seq(Seq2,Seq2First,Seq2Last),
        edit(Seq1First,Seq2First,FirstDist),
        edit(Seq1Last,Seq2Last,LastDist),
        Ratio is 1 + LastDist / 1 + FirstDist,
        align_pairs_ratio(SeqRest,RatiosRest).

split_seq(Seq,Part1,Part2) :-
        length(Seq,SeqLen),
        HalfLen is SeqLen // 2,
        length(Part1,HalfLen),
        length(Part2,HalfLen),
        append(Part1,Part2,Seq).



align_pairs_pmcomp([],[]).
align_pairs_pmcomp([[Seq1,Seq2]|SeqRest],[Score|ScoresRest]) :-
        write('.'),
        pmcomp_align(Seq1,Seq2,Score),
        align_pairs_pmcomp(SeqRest,ScoresRest).

/*
%% Dummy implementation - substitute with real one when ready
align_pairs_pmcomp([],[]).
align_pairs_pmcomp([[_Seq1,_Seq2]|SeqRest],[1|ScoresRest]) :-
        write('.'),
        %pmcomp_align(Seq1,Seq2,Score),
        align_pairs_pmcomp(SeqRest,ScoresRest).
*/

hamming([],[],0).
hamming([X|Xs],[X|Ys],Dist) :- hamming(Xs,Ys,Dist).
hamming([X|Xs],[Y|Ys],Dist) :- X \= Y, hamming(Xs,Ys,Dist1), Dist is 1 + Dist1.

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
