%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% NOM ET VERSION :
%
%      script_stats.pl -- Version 0.0
%
% GOAL :
%       Several scripts to compute statistics
%     
% HISTORIQUE :
%	M.P	28/04/2010
%
% DESCRIPTION :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compute_stats(Type_Codon,Options,[],[]) :-
        !.


compute_stats(Type,Options,List_Data,Stats) :-
        compute_stats_rec(Type_Codon,Options,List_Data,Counting_List,Stats).


% Recursiv call
compute_stats_rec(Type_Codon,Options,[],Counting_List,Stats) :-
        normalize(Type_Codon,Counting_List,Stats).

compute_stats_rec(Type_Codon,Options,[Data|Rest_Data],Counting_List,Stats) :-
        stats(Type_Codon,Options,Data,Counting_List,Counting_List2),
        compute_stats_rec(Type_Codon,Options,Rest_Data,Counting_List2,Stats).



%%%%
% Data Manipulation
%%%%


% build_list_codon(++Data,--Resultat)
% Translate a list of nucleotids into a list of codon
% Precondition: Data length should be dividable by 3
build_list_codon([],[]) :-
        !.

build_list_codon([Data|Rest_Data],Resultat) :-
        (build_list_codon_rec(Data,Codon_Data) ->
            Resultat = [Codon_Data|Rest]
        ;
            write('List of Nucleotid not divided by 3'),
            Resultat = Rest
        ),
        build_list_codon(Rest_Data,Rest).
        

% Recursiv call
build_list_codon_rec([],[]) :-
        !.
        
        
build_list_codon_rec([N1,N2,N3|Rest],[[N1,N2,N3]|Rest_Codons]) :-
        !,
        build_list_codon_rec(Rest,Rest_Codons).




% build_list_amino(++Data,--Resultat)
% Translate a list of condons into a list of animo acid
% Precondition: Data length should be dividable by 3

build_list_amino([],[]) :-
        !.


build_list_amino([Data|Rest_Data],[Codon_Data|Rest]) :-
        build_list_amino_rec(Data,Codon_Data),
        build_list_amino(Rest_Data,Rest).


% Recursiv call
build_list_amino_rec([],[]) :-
        !.

build_list_amino_rec([Codon|Rest_Codons],[Amino|Rest_Amino]) :-
        codon2amino(Codon,Amino),
        build_list_amino_rec(Rest_Codons,Rest_Amino).




% Utils Data transformation

% Direct translation of a codon into an amino acid
codon2amino([a,a,a],k) :- !.
codon2amino([a,a,c],n) :- !.
codon2amino([a,a,g],k) :- !.
codon2amino([a,a,t],n) :- !.
codon2amino([a,c,a],k) :- !.
codon2amino([a,c,c],t) :- !.
codon2amino([a,c,g],t) :- !.
codon2amino([a,c,t],t) :- !.
codon2amino([a,g,a],r) :- !.
codon2amino([a,g,c],s) :- !.
codon2amino([a,g,g],r) :- !.
codon2amino([a,g,t],s) :- !.
codon2amino([a,t,a],i) :- !.
codon2amino([a,t,c],i) :- !.
codon2amino([a,t,g],m) :- !.
codon2amino([a,t,t],i) :- !.
codon2amino([c,a,a],q) :- !.
codon2amino([c,a,c],h) :- !.
codon2amino([c,a,g],q) :- !.
codon2amino([c,a,t],h) :- !.
codon2amino([c,c,a],p) :- !.
codon2amino([c,c,c],p) :- !.
codon2amino([c,c,g],p) :- !.
codon2amino([c,c,t],p) :- !.
codon2amino([c,g,a],r) :- !.
codon2amino([c,g,c],r) :- !.
codon2amino([c,g,g],r) :- !.
codon2amino([c,g,t],r) :- !.
codon2amino([c,t,a],l) :- !.
codon2amino([c,t,c],l) :- !.
codon2amino([c,t,g],l) :- !.
codon2amino([c,t,t],l) :- !.
codon2amino([g,a,a],e) :- !.
codon2amino([g,a,c],d) :- !.
codon2amino([g,a,g],e) :- !.
codon2amino([g,a,t],d) :- !.
codon2amino([g,c,a],a) :- !.
codon2amino([g,c,c],a) :- !.
codon2amino([g,c,g],a) :- !.
codon2amino([g,c,t],a) :- !.
codon2amino([g,g,a],g) :- !.
codon2amino([g,g,c],g) :- !.
codon2amino([g,g,g],g) :- !.
codon2amino([g,g,t],g) :- !.
codon2amino([g,t,a],v) :- !.
codon2amino([g,t,c],v) :- !.
codon2amino([g,t,g],v) :- !.
codon2amino([g,t,t],v) :- !.
codon2amino([t,a,a],*) :- !.
codon2amino([t,a,c],y) :- !.
codon2amino([t,a,g],*) :- !.
codon2amino([t,a,t],y) :- !.
codon2amino([t,c,a],s) :- !.
codon2amino([t,c,c],s) :- !.
codon2amino([t,c,g],s) :- !.
codon2amino([t,c,t],s) :- !.
codon2amino([t,g,a],*) :- !.
codon2amino([t,g,c],c) :- !.
codon2amino([t,g,g],w) :- !.
codon2amino([t,g,t],c) :- !.
codon2amino([t,t,a],l) :- !.
codon2amino([t,t,c],f) :- !.
codon2amino([t,t,g],l) :- !.
codon2amino([t,t,t],f) :- !.

