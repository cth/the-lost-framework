go :-
	load_clauses('25_best_raw.pl',Clauses),
	tell('clusters_table1.csv'),
	report_clusters(1,Clauses),
	told,
	tell('cluster_hits.csv'),
	report_cluster_hits(1,Clauses),
	told.

report_clusters(_N,[]).
report_clusters(N,[cluster(Features,Members)|Rest]) :-
	RelevantFeatures = [ organisms, diversity, pmcomp, codon_score, avg_upstream_uag ], 
	findall(FeatureValue,(member(Feature,RelevantFeatures),extract_feature_value(Feature,Features,FeatureValue)),FeatureValues),
	write(N),write(','),
	foreach(F in FeatureValues, (write(F), write(','))),
	nl,
	N1 is N + 1,
	report_clusters(N1,Rest).
	
report_cluster_hits(_N,[]).
report_cluster_hits(N,[cluster(Features,Members)|Rest]) :-
	foreach(M in Members,(write(N),write(','),foreach(E in M, (write(E),write(','))),nl)),
	N1 is N + 1,	
	report_cluster_hits(N1,Rest).
	
extract_feature_value(Feature,Features,Value) :-
	Match =.. [ Feature, Value ],
	member(Match,Features).
