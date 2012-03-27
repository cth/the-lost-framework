:- [union].


:- dynamic lookup_match/2.
:- dynamic uag_position/2.

:- use(genedb).

assert_once(Term) :-	(Term -> true ; assert(Term)).

build_links(InputFile) :-
	open(InputFile,read,IS),
	build_links_rec(1,IS),
	nl,
	close(IS).
	
translate_name('Thermincola potens','Thermincola_potens') :- !.
translate_name('Acetohalobium arabaticum','Acetohalobium_arabaticum') :- !.
translate_name(X,X).
	
build_links_rec(I,IS) :-
	((0 is I mod 100) -> write(I) ; write('.')),
	read(IS,Term),
	((Term == end_of_file) ->
		true
		;
%		Term = blast_match(OrganismA_1,LeftA,RightA,StrandA,FrameA,ExtraA),
		Term = blast_match(_,LeftA,RightA,StrandA,FrameA,ExtraMatch),
		member(match_to(orf(OrganismB_1,LeftB,RightB,StrandB,FrameB,ExtraB)), ExtraMatch),
		member(query_orf(orf(OrganismA_1, LeftA, RightA, StrandA, FrameA,ExtraA)), ExtraMatch),
		translate_name(OrganismA_1,OrganismA),
		translate_name(OrganismB_1,OrganismB),
		makeset([OrganismA,LeftA,RightA,StrandA,FrameA]),
		makeset([OrganismB,LeftB,RightB,StrandB,FrameB]),
		union([OrganismA,LeftA,RightA,StrandA,FrameA],[OrganismB,LeftB,RightB,StrandB,FrameB]),
		% Associate blast match Term with organism A
		assert_once(lookup_match([OrganismA,LeftA,RightA,StrandA,FrameA],Term)),
		assert_once(lookup_match([OrganismB,LeftB,RightB,StrandB,FrameB],Term)),
		member(in_frame_stops([UAG_A]),ExtraA),
		assert_once(uag_position([OrganismA,LeftA,RightA,StrandA,FrameA],UAG_A)),
		member(in_frame_stops([UAG_B]),ExtraB),
		assert_once(uag_position([OrganismB,LeftB,RightB,StrandB,FrameB],UAG_B)),		
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
	open(ClusterFile,write,SimpleClusterStream),
	open(DetailedClusterFile,write,DetailClusterStream),
	writeln('writing clusters to output files...'),
	report_clusters(1,Clusters,SimpleClusterStream,DetailClusterStream),
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

multi_organism_clusters([],[]).

multi_organism_clusters([cluster(Cluster)|Clusters],[cluster(Cluster)|MultiOrganismClusters]) :-
	findall(Organism,member([Organism|_],Cluster),Organisms), % Only one organism
	eliminate_duplicate(Organisms,NoDups),
	length(NoDups,Num),
	Num > 1,
	!,
	multi_organism_clusters(Clusters,MultiOrganismClusters).

multi_organism_clusters([_Cluster|Clusters],MultiOrganismClusters) :-
	multi_organism_clusters(Clusters,MultiOrganismClusters).

report_multi_organism_clusters(InputFile,OutputFile) :-
	terms_from_file(InputFile,Clusters),
	multi_organism_clusters(Clusters,MultiClusters),
	terms_to_file(OutputFile,MultiClusters).
	
test_multi :-
	report_multi_organism_clusters('all.pl','multi.pl').
		
test :-
	hit_closure('data1000_trim.pl','clusters.pl','cluster_matches.pl').
