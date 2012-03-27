:- ['../../lost.pl'].


	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Rank by size
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
rank_by_size(ClustersFile,ClustersDetailFile,SortedClustersFile,SortedClustersDetailFile) :-
	terms_from_file(ClustersFile,Clusters),
	sort_clusters_by_size(Clusters,SortedClusters),
	terms_to_file(SortedClustersFile,SortedClusters),
	[ClustersDetailFile],
	findall(cluster(Cluster,Details),(member(Cluster,SortedClusters), cluster_matches(Cluster,Details)),SortedClustersDetail),
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
	findall(cluster(Cluster,Details),(member(Cluster,SortedClusters), cluster_matches(Cluster,Details)),SortedClustersDetail),
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
	findall(cluster(Cluster,Details),(member(Cluster,SortedClusters), cluster_matches(Cluster,Details)),SortedClustersDetail),
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

test :- 
	rank_by_average_length('clusters.pl','cluster_matches.pl','clusters_sorted.pl','clusters_detail_sorted.pl').

test2 :- 
	rank_by_size('clusters.pl','cluster_matches.pl','clusters_sorted.pl','clusters_detail_sorted.pl').
	
test3 :-
	rank_by_organisms('clusters.pl','cluster_matches.pl','clusters_sorted.pl','clusters_detail_sorted.pl').
	
			