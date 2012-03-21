:- table tlink/2.

tlink(A,B) :-
	link(A,B).

tlink(A,B) :-
	link(B,A).
	
tlink(A,C) :-
	link(A,B),
	tlink(B,C).

link([OrganismA,LeftA,RightA,StrandA,FrameA],[OrganismB,LeftB,RightB,StrandB,FrameB]) :-
	blast_match(OrganismAFullname,LeftA,RightA,StrandA,FrameA,ExtraA),
	member(match_to(orf(OrganismB,LeftB,RightB,StrandB,FrameB,ExtraB)), ExtraA),
	extract_organism_name(OrganismAFullname,OrganismA).
	
clusters(Clusters) :-
	findall(A,tlink(_,A),ClustersDup),
	eliminate_duplicate(ClustersDup,Clusters).

/*	
hit_closure(HitsFile,ClosureFile) :-
	[HitsFile], % Load the HitsFile into memory
	
cluster_hits([Hit|HitRest],Clusters) :-
	member(Cluster,Clusters),
	match_to_hit(Hit,Cluster),


test :-
	hit_closure('data10.pl').
*/	


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
