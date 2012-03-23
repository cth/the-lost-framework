:- [union].

%:- ['../../lost.pl'].

:- dynamic lookup_match/2.
:- dynamic uag_position/2.

:- use(genedb).

assert_once(Term) :-	(Term -> true ; assert(Term)).

build_links(InputFile) :-
	open(InputFile,read,IS),
	build_links_rec(1,IS),
	nl,
	close(IS).
	
build_links_rec(I,IS) :-
	((0 is I mod 100) -> write(I) ; write('.')),
	read(IS,Term),
	((Term == end_of_file) ->
		true
		;
		Term = blast_match(OrganismA,LeftA,RightA,StrandA,FrameA,ExtraA),
		member(match_to(orf(OrganismB,LeftB,RightB,StrandB,FrameB,ExtraB)), ExtraA),
		makeset([OrganismA,LeftA,RightA,StrandA,FrameA]),
		makeset([OrganismB,LeftB,RightB,StrandB,FrameB]),
		union([OrganismA,LeftA,RightA,StrandA,FrameA],[OrganismB,LeftB,RightB,StrandB,FrameB]),
		% Associate blast match Term with organism A
		assert_once(lookup_match([OrganismA,LeftA,RightA,StrandA,FrameA],Term)),
		assert_once(lookup_match([OrganismB,LeftB,RightB,StrandB,FrameB],Term)),
		member(in_frame_stops([UAG]),ExtraB),
		assert_once(uag_position([OrganismB,LeftB,RightB,StrandB,FrameB],UAG)),
		I1 is I + 1,
		build_links_rec(I1,IS)).

clusters(Clusters) :-
	roots(Roots),
	clusters_rec(1,Roots,Clusters).

clusters_rec(_,[],[]) :- nl.

clusters_rec(I,[Root|RootsRest],[Cluster|ClustersRest]) :-
	((0 is I mod 10) -> write(I) ; write('.')),
	transitive_closure(Root,Cluster),
	I1 is I + 1,
	!,
	clusters_rec(I1,RootsRest,ClustersRest).
	
sort_clusters_by_size(Clusters,ClustersBySizeDescending) :-
	add_cluster_size(Clusters,ClustersWithSize),
	sort(ClustersWithSize,ClustersWithSizeBySize),
	add_cluster_size(ClustersBySizeAscending,ClustersWithSizeBySize),
	reverse(ClustersBySizeAscending,ClustersBySizeDescending).

add_cluster_size([],[]).
add_cluster_size([Cluster|ClusterRest],[[Size,Cluster]|SizeClusterRest]) :-
	length(Cluster,Size),
	add_cluster_size(ClusterRest,SizeClusterRest).
	
add_uag_positions([],[]).
add_uag_positions([C|Cs],[D|Ds]) :-
	(uag_position(C,UAG) ->
		append(C,[UAG],D)
		;
		append(C,[na],D)),
	add_uag_positions(Cs,Ds).


hit_closure(HitsFile,ClusterFile,DetailedClusterFile) :-
	writeln('reading matches and building union-find data structure:'),
	build_links(HitsFile),!,
	writeln('building clusters:'),	
	clusters(Clusters),
	writeln('sorting clusters by size..'),
	sort_clusters_by_size(Clusters,SortClusters),
	open(ClusterFile,write,SimpleClusterStream),
	open(DetailedClusterFile,write,DetailClusterStream),
	writeln('writing clusters to output files...'),
	report_clusters(1,SortClusters,SimpleClusterStream,DetailClusterStream),
	close(SimpleClusterStream),
	close(DetailClusterStream).
	
report_clusters(_,[],_,_).

report_clusters(I,[Cluster|ClustersRest],SimpleClusterStream,DetailClusterStream) :-
	((0 is I mod 10) -> write(I) ; write('.')),
	findall(FullMatch,(member(Match,Cluster),lookup_match(Match,FullMatch)),FullMatches),
	eliminate_duplicate(FullMatches,UniqMatches),
	add_uag_positions(Cluster,ClusterWithUAG),
	(is_ribosomal_cluster(UniqMatches) ->
		true
		;
		writeq(SimpleClusterStream,cluster(ClusterWithUAG)),
		write(SimpleClusterStream,'.\n'),
		writeq(DetailClusterStream,cluster_matches(Cluster,UniqMatches)),
		write(DetailClusterStream,'.\n')),
	!,
	I1 is I + 1,
	report_clusters(I1,ClustersRest,SimpleClusterStream,DetailClusterStream).
	
is_ribosomal_cluster(Matches) :-
	member(Match,Matches),
	gene_extra_field(Match,rna_overlap,_).

test :-
	hit_closure('data1000_trim.pl','clusters.pl','cluster_matches.pl').
