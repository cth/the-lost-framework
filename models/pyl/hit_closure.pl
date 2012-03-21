:- table tlink/2.

tlink(A,B) :-
	link(A,B).

tlink(A,B) :-
	link(B,A).

tlink(A,C) :-
	tlink(A,B),
	tlink(B,C).

link([OrganismA,LeftA,RightA,StrandA,FrameA],[OrganismB,LeftB,RightB,StrandB,FrameB]) :-
	blast_match(OrganismAFullname,LeftA,RightA,StrandA,FrameA,ExtraA),
	member(match_to(orf(OrganismB,LeftB,RightB,StrandB,FrameB,_ExtraB)), ExtraA),
	extract_organism_name(OrganismAFullname,OrganismA).
	
clusters(Clusters) :-
	findall(C,cluster(C),ClustersDup),
	length(ClustersDup,DupLen),
	writeln(duplen(DupLen)),
	!,
	eliminate_duplicate(ClustersDup,Clusters),
	length(Clusters,CLen),
	writeln(clusters(CLen)).
	
cluster(SortC) :-
	tlink(A,_),
	findall(B,tlink(A,B),C1),
	eliminate_duplicate(C1,C),
	sort(C,SortC).
	
hit_closure(HitsFile,ClusterFile) :-
	[HitsFile], % Load the HitsFile into memory
	clusters(Clusters),
	open(ClusterFile,write,ClusterStream),
	forall(member(Cluster,Clusters), (writeq(ClusterStream,cluster(Cluster)),write(ClusterStream,'.\n'))),
	close(ClusterStream).

test :-
	hit_closure('data100.pl','clusters.pl').

extract_organism_name(SeqId,JustName) :-
	atom_codes(SeqId,Codes),
	parse_name_pos(NameCodes,Codes,[]),!,
	atom_codes(JustName,NameCodes).

parse_name_pos(Name) -->
	name(Name),
	position.

name([]) --> [].
name([95|Xs]) --> " ", name(Xs). % Convert spaces to underscores
name([X|Xs]) --> [X], name(Xs).

position -->
	"_",
   digits,
   "_",
   digits,
   "_",
   strand,
   "_",
   frame.

digits --> [].
digits -->
   digit,
	digits.
	
digit --> "0" ; "1" ; "2" ; "3" ; "4" ; "5" ; "6" ; "7" ; "8" ; "9".
strand --> "+" ; "-".
frame --> "1" ; "2" ; "3".

remove_spaces(SeqId,SeqIdTrim) :-
	atom_codes(SeqId,Codes),
	delete(Codes,32,Codes1),
	atom_codes(SeqIdTrim,Codes1).
