:- [union].

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
		member(match_to(orf(OrganismB,LeftB,RightB,StrandB,FrameB,_ExtraB)), ExtraA),
		makeset([OrganismA,LeftA,RightA,StrandA,FrameA]),
		makeset([OrganismB,LeftB,RightB,StrandB,FrameB]),
		union([OrganismA,LeftA,RightA,StrandA,FrameA],[OrganismB,LeftB,RightB,StrandB,FrameB]),
		I1 is I + 1,
		build_links_rec(I1,IS)).
	
clusters(Clusters) :-
	roots(Roots),
	clusters_rec(1,Roots,Clusters).

clusters_rec(I,[],[]) :- nl.

clusters_rec(I,[Root|RootsRest],[Cluster|ClustersRest]) :-
	((0 is I mod 10) -> write(I) ; write('.')),
	transitive_closure(Root,Cluster),
	I1 is I + 1,
	!,
	clusters_rec(I1,RootsRest,ClustersRest).

hit_closure(HitsFile,ClusterFile) :-
	writeln('reading matches and building union-find data structure:'),
	build_links(HitsFile),!,
	writeln('building clusters:'),	
	clusters(Clusters),
	open(ClusterFile,write,ClusterStream),
	forall(member(Cluster,Clusters), (writeq(ClusterStream,cluster(Cluster)),write(ClusterStream,'.\n'))),
	close(ClusterStream).

test :-
	hit_closure('data1000_trim.pl','clusters.pl').