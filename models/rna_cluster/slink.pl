%:- use_module(library(chr)).
%:- set_prolog_flag(chr_toplevel_show_store,false).
%:- chr_option(optimize,full).

%% Single-linkage clustering algorithm, implemented in Constraint Handling Rules
%
% Will build a dendogram as distance/3 constraints are inserted into the constraint store.
% The dendogram is built in the order the distance constraints are inserted into the store.
% The usual single-linkage algorithm is achieved if the distance/3 constraints are inserted 
% in sorted (ascending) order. 
% 
% @author Christian Theil Have

:- chr_constraint member(+,+), active_member(+,+), distance(+,+,+), branch(+,+,+), leaf(+), node(+,+), largest_cluster_id(+).
:- chr_constraint build_tree/0, init_slink/0, extract_trees/1.

% Initialize constraint with initial ID
init_slink <=>  largest_cluster_id(0).
	
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
	node(C,[[CostAC,A,TreeA],[CostBC,B,TreeB]]).

extract_trees(Stream) \ node(Id,Branches) <=> write_newick_tree(Stream,[0,Id,Branches]) | true.
