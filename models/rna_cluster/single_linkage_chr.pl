:- use_module(library(chr)).
:- set_prolog_flag(chr_toplevel_show_store,false).
:- chr_option(optimize,full).

%% Single-linkage clustering algorithm, implemented in Constraint Handling Rules
%
% Will build a dendogram as distance/3 constraints are inserted into the constraint store.
% The dendogram is built in the order the distance constraints are inserted into the store.
% The usual single-linkage algorithm is achieved if the distance/3 constraints are inserted 
% in sorted (ascending) order. 
% 
% @author Christian Theil Have

:- chr_constraint member(+,+), active_member(+,+), distance(+,+,+), branch(+,+,+), leaf(+), node(+,+), largest_cluster_id(+).
:- chr_constraint build_tree/0.

init_single_linkage :-
	largest_cluster_id(0).

% A and B are allready in same cluster due to transitive closure properties.
active_member(SameCluster,A), active_member(SameCluster,B) \ distance(A,B,_Cost) <=> true.

% Promote members to active when they are created
member(Cluster,Member) ==> active_member(Cluster,Member).

% Both A and B exist in clusters. Merge those clusters.
% We have a distance between two "live" clusters: Merge the clusters.
active_member(ClusterOne,A), active_member(ClusterTwo,B) \ distance(A,B,Cost), largest_cluster_id(LargestClusterId) <=>
	NextClusterId is 1 + LargestClusterId
	|
	branch(ClusterOne,NextClusterId,Cost),
	branch(ClusterTwo,NextClusterId,Cost),
	largest_cluster_id(NextClusterId).

% A is in a cluster. Create new cluster for B.
active_member(_,A) \ distance(A,B,Cost)  <=>
	member(B,B),
	distance(A,B,Cost).

% B is in a cluster. Create new cluster for A.
active_member(_,B) \ distance(A,B,Cost) <=>
	member(A,A),
	distance(A,B,Cost).

distance(A,B,Cost) <=>
	member(A,A),
	member(B,B),
	distance(A,B,Cost).

% When merging clusters into new clusters, promote active members of old 
% clusters to the active members of the new clusters
branch(OldCluster,NewCluster,_) \ active_member(OldCluster,Member) <=>
	active_member(NewCluster,Member).

% Initially, assume that all nodes are leafs.
branch(A,_,_) ==> leaf(A).

% If a node has a child node, it is not a leaf.
branch(_,A,_) \ leaf(A) <=> true.

build_tree \ leaf(A) <=> node(A,[]).

build_tree \ node(A,TreeA), node(B,TreeB), branch(A,C,CostAC), branch(B,C,CostBC)  <=>
	write('.'),flush
	|
	node(C,[[CostAC,A,TreeA],[CostBC,B,TreeB]]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
% A DCG to convert trees to newick format
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

% A Leaf node
newick([Dist,NodeId,[]]) --> nodeid(NodeId), ":", str(Dist).

% A Junction 
newick([DistParent,_NodeId,[LeftSubTree,RightSubTree]]) --> 
	"(",
	newick(LeftSubTree),
	",",
	newick(RightSubTree),
	")",
	":",
	str(DistParent).
	
nodeid(Num) --> str(Num).
	
nodeid((Organism,LeftPos,RightPos)) -->
	{
		atom_codes(Organism,OrganismCodes)
	},
	OrganismCodes,
	"_",
	str(LeftPos),
	"-",
	str(RightPos).
	

str(Num) -->
	{
		number(Num),
		number_chars(Num,Chars),
		to_atom_codes(Chars,Codes)
	},
	Codes.

to_atom_codes([],[]).
to_atom_codes([C|Cs], [D|Ds]) :- atom_codes(C,[D]), to_atom_codes(Cs,Ds).


test_slc :-
	init_single_linkage,
	distance(a,b,1),
	distance(c,d,1),
	distance(a,c,2),
	distance(b,d,1),
	build_tree,
	find_chr_constraint(node(Root,Tree)),
	writeln('TREE: '), 
	writeln([0,Root,Tree]),
	!,
	writeln(newick([0,Root,Tree],AsNewick,[])),
	newick([0,Root,Tree],AsNewick,[]),
	atom_codes(NewickA,AsNewick),
	writeln(NewickA).

build_trees_from_distances(DistancesFile,NewickTreesFile) :-
	init_single_linkage,
	write('Reading distances: '),
	open(DistancesFile,read,Stream),
	read_from_distance_matrix(Stream,0),
	close(Stream), 
	writeln('done.'),
	writeln('Building trees...'),
	build_tree,
	writeln('done.'),
	writeln('extracting trees:'),
	findall([0,Root,Tree],find_chr_constraint(node(Root,Tree)),Trees),
	length(Trees,NumTrees),
	write('Built '), write(NumTrees), writeln(' trees.'), nl,
	open(NewickTreesFile,write,OutS),
	write_trees_newick(Trees,OutS),
	close(OutS).
	
write_trees_newick([],_).
write_trees_newick([T|Ts],Stream) :-
	newick(T,NewickT,[]),
	atom_codes(NewickA,NewickT),
	write(Stream,NewickA),
	write(Stream,';\n'),
	write_trees_newick(Ts,Stream).

read_from_distance_matrix(Stream,Count) :-
	read(Stream,Term),
	((Term == end_of_file) ->
                write('reached end of file'),nl,
		true
		;
                Term = [Distance,A,B],
		((0 is Count mod 100) -> write('.'), flush ; true),
		((0 is Count mod 1000) -> write(Count), flush ; true),
		Count1 is Count + 1,
		distance(A,B,Distance),
		read_from_distance_matrix(Stream,Count1)
	).
	
go :-
	build_trees_from_distances('distance_matrix.pl','trees.newick').

