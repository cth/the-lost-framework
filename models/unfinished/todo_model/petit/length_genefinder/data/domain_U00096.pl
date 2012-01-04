%% get_domain(+Duration_Type,-Domain)
%
% Building of the domain of the duration random variable with respect with Duration
% type

% default value: all_genome
get_domain('all_genome',L) :-
        !,
        get_domain_length(1,2359,L).

% Gene difficulty parameters (setting for E.Coli and eg and gm as genefinder)
get_domain(gene_difficulty('hard'),L) :-
        !,
        get_domain_length(1,1518,L).

get_domain(gene_difficulty('medium'),L) :-
        !,
        get_domain_length(1,2359,L).

get_domain(gene_difficulty('easy'),L) :-
        !,
        get_domain_length(1,1654,L).



% Option range(Min,Max) used to set the duration
get_domain(range([Min,Max]),L) :-
        !,
        Min3 is ceiling(Min/3),
        Max3 is floor(Max/3),
        get_domain_length(Min3,Max3,L).



%---
% Utils
%---

%% get_domain_length(+Min,+Max,-Domain)
%
% Generate of list values between Min and Max
get_domain_length(Max,Max,[Max]) :-
        !.

get_domain_length(Min,Max,[Min|Rest]) :-
        !,
        Min1 is Min+1,
        get_domain_length(Min1,Max,Rest).

