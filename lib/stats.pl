%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% NOM ET VERSION :
%
%      stats.pl -- Version 0.0
%
% GOAL :
%       Computation of several statistics 
%     
% HISTORIQUE :
%	M.P	10/03/2010
%
% DESCRIPTION :
%
%%%%


% stats(++Data_Type,++Options,++Data,??Result).
% stats(++Data_Type,++Options,++Data,++Input_Counting,--Past,??Result).
% Assumption: Data format corresponds to the kind of statistics that you compute
% List of nucleotids for nucleotids-codon stats
% List of AminoAcids for Amino stats
% List of Ranges for Length stats computation
% Option = Order
:- lost_include_api(interface).


% Type nucleotide, codon, amino_acid
stats(Type,Options,Data,Input_Counting,Result) :-
        member(Type,[nucleotide,codon,amino_acid]),
        var(Input_Counting),
        !,
        (member(order(N),Options) ->
            true
        ;
            N =0
        ),
        (member(past(Past),Options) ->
            Data  = New_Data
        ;
            check_or_fail(build_past(N,Data,Past-Past,New_Data),
                          stats_error('Data too short for the order')
                         )
        ),
        init_counting(Type,N,Init_Counting),
        stats_rec(New_Data,Past,Init_Counting ,Result).
        
        
stats(Type,Options,Data,Input_Counting,Result) :-
        member(Type,[nucleotide,codon,amino_acid]),
        !,
        (member(order(N),Options) ->
            true
        ;
            N =0
        ),
        (member(past(Past),Options) ->
            Data  = New_Data
        ;
            check_or_fail(build_past(N,Data,Past-Past,New_Data),
                          stats_error('Data too short for the order')
                         )
        ),
        stats_rec(New_Data,Past,Input_Counting,Result).


% Stats that records the past

stats(Type,Options,Data,Input_Counting,Past_Final,Result) :-
        member(Type,[nucleotide,codon,amino_acid]),
        var(Input_Counting),
        !,
        (member(order(N),Options) ->
            true
        ;
            N =0
        ),
        (member(past(Past),Options) ->
            Data  = New_Data
        ;
            check_or_fail(build_past(N,Data,Past-Past,New_Data),
                          stats_error('Data too short for the order')
                         )
        ),
        init_counting(Type,N,Init_Counting),
        stats_rec(New_Data,Past,Init_Counting,Past_Final,Result).




stats(Type,Options,Data,Input_Counting,Past_Final,Result) :-
        member(Type,[nucleotide,codon,amino_acid]),
        !,
        (member(order(N),Options) ->
            true
        ;
            N =0
        ),
        (member(past(Past),Options) ->
            Data  = New_Data
        ;
            check_or_fail(build_past(N,Data,Past-Past,New_Data),
                          stats_error('Data too short for the order')
                         )
        ),
        stats_rec(New_Data,Past,Input_Counting,Past_Final,Result).


stats(length,Ranges,Result) :-
        stats_length(Ranges,[],Result).

% Recursive call

stats_rec([],_Past,Result,Result) :-
        !.


stats_rec([Datum|Rest_Data],Past,Input_Counting,Result) :-
        update_counting(Datum,Past,Input_Counting,Input_Counting2),
        (Past = [] ->
            true
        ;
            Past = [_|Rest_Past],
            append(Rest_Past,[Datum],New_Past)
        ),
        stats_rec(Rest_Data,New_Past,Input_Counting2,Result).


% Recursive call when the pas is recorded

stats_rec([],Past,Result,Past,Result) :-
        !.

stats_rec([Datum|Rest_Data],Past,Input_Counting,Past_Final,Result) :-
        update_counting(Datum,Past,Input_Counting,Input_Counting2),
        (Past = [] ->
            true
        ;
            Past = [_|Rest_Past],
            append(Rest_Past,[Datum],New_Past)
        ),
        stats_rec(Rest_Data,New_Past,Input_Counting2,Past_Final,Result).




% Recursive call to compute statistics on the length distribution

stats_length([],Result,Result) :-
        !.

stats_length([[Min,Max]|Ranges],Counting,Result) :-
        Length is Max-Min+1,
        update_length(Length,Counting,Counting_New),
        stats_length(Ranges,Counting_New,Result).


% init_counting
%--------
% Remark: Exponential explosion here: take care to select a
% reasonable order (especially for codon type

init_counting(Type,0,[([],Init_List)]) :-
        !,
        get_init(Type,Init_List).


init_counting(Type,Order,Init_List) :-
        findall(Res,init_counting_rec(Type,Order,Res),Init_List).


init_counting_rec(Type,1,([Past],Init_List)) :-
        !,
        get(Type,Past),
        get_init(Type,Init_List).

init_counting_rec(Type,Order,([Past|Rest],Nuc_Init)) :-
        get(Type,Past),
        Order1 is Order-1, 
        init_counting_rec(Type,Order1,(Rest,Nuc_Init)).
        



% update_counting(++Datum,++Past,++Input_Counting,--Counting_Update)
% Remark: failed if the past is not in Input_Counting
% Reminder: format of Input Counting Elt: (Past,[(Values,Count),Values,Count), ...])


update_counting(_Datum,_Past,[],[]) :-
        !,
        fail.

update_counting(Datum,Past,[(Past,Counting)|Rest],[(Past,Counting_Update)|Rest]) :-
        !,
        update_counting_2(Datum,Counting,Counting_Update).

update_counting(Datum,Past,[Head|Rest],[Head|Rest_Update]) :-
        !,
        update_counting(Datum,Past,Rest,Rest_Update).


% Phase 2 of counting

update_counting_2([],[]) :-
        !,
        fail.

update_counting_2(Datum,[(Datum,Count)|Rest],[(Datum,New_Count)|Rest]):-
        !,
        New_Count is Count+1.
        
        
update_counting_2(Datum,[Head|Rest],[Head|Rest_Update]) :-
        !,
        update_counting_2(Datum,Rest,Rest_Update).



% update_length(++Length,++Couting,--Counting_New).


update_length(Length,[],[(Length,1)]) :-
        !.

update_length(Length,[(Length,Count)|Rest],[(Length,New_Count)|Rest]) :-
        !,
        New_Count is Count+1.

update_length(Length,[(Length_Below,Count)|Rest],[(Length_Below,Count)|Rest_Update]) :-
        Length_Below < Length,
        !,
        update_length(Length,Rest,Rest_Update).

% First length above length
update_length(Length,[(Length_Above,Count)|Rest],[(Length,1),(Length_Above,Count)|Rest]) :-
        Length < Length_Above,
        !.


% get_init/2


get_init(nucleotide,[(a,0),(c,0),(g,0),(t,0)]) :-
        !.

get_init(codon,[([a,a,a],0),([a,a,c],0),([a,a,g],0),([a,a,t],0),
                ([a,c,a],0),([a,c,c],0),([a,c,g],0),([a,c,t],0),
                ([a,g,a],0),([a,g,c],0),([a,g,g],0),([a,g,t],0),
                ([a,t,a],0),([a,t,c],0),([a,t,g],0),([a,t,t],0),
                ([c,a,a],0),([c,a,c],0),([c,a,g],0),([c,a,t],0),
                ([c,c,a],0),([c,c,c],0),([c,c,g],0),([c,c,t],0),
                ([c,g,a],0),([c,g,c],0),([c,g,g],0),([c,g,t],0),
                ([c,t,a],0),([c,t,c],0),([c,t,g],0),([c,t,t],0),
                ([g,a,a],0),([g,a,c],0),([g,a,g],0),([g,a,t],0),
                ([g,c,a],0),([g,c,c],0),([g,c,g],0),([g,c,t],0),
                ([g,g,a],0),([g,g,c],0),([g,g,g],0),([g,g,t],0),
                ([g,t,a],0),([g,t,c],0),([g,t,g],0),([g,t,t],0),
                ([t,a,a],0),([t,a,c],0),([t,a,g],0),([t,a,t],0),
                ([t,c,a],0),([t,c,c],0),([t,c,g],0),([t,c,t],0),
                ([t,g,a],0),([t,g,c],0),([t,g,g],0),([t,g,t],0),
                ([t,t,a],0),([t,t,c],0),([t,t,g],0),([t,t,t],0)
               ]) :-
        !.


get_init(amino_acid,[(a,0),(c,0),(d,0),(e,0),
                     (f,0),(g,0),(h,0),(i,0),
                     (k,0),(l,0),(m,0),(n,0),
                     (p,0),(q,0),(r,0),(s,0),
                     (t,0),(v,0),(w,0),(y,0)
                    ]):-
        !.


% get(++Type,--Values).

get(nucleotide,a).
get(nucleotide,c).
get(nucleotide,g).
get(nucleotide,t).

get(codon,[a,a,a]).
get(codon,[a,a,c]).
get(codon,[a,a,g]).
get(codon,[a,a,t]).
get(codon,[a,c,a]).
get(codon,[a,c,c]).
get(codon,[a,c,g]).
get(codon,[a,c,t]).
get(codon,[a,g,a]).
get(codon,[a,g,c]).
get(codon,[a,g,g]).
get(codon,[a,g,t]).
get(codon,[a,t,a]).
get(codon,[a,t,c]).
get(codon,[a,t,g]).
get(codon,[a,t,t]).
get(codon,[c,a,a]).
get(codon,[c,a,c]).
get(codon,[c,a,g]).
get(codon,[c,a,t]).
get(codon,[c,c,a]).
get(codon,[c,c,c]).
get(codon,[c,c,g]).
get(codon,[c,c,t]).
get(codon,[c,g,a]).
get(codon,[c,g,c]).
get(codon,[c,g,g]).
get(codon,[c,g,t]).
get(codon,[c,t,a]).
get(codon,[c,t,c]).
get(codon,[c,t,g]).
get(codon,[c,t,t]).
get(codon,[g,a,a]).
get(codon,[g,a,c]).
get(codon,[g,a,g]).
get(codon,[g,a,t]).
get(codon,[g,c,a]).
get(codon,[g,c,c]).
get(codon,[g,c,g]).
get(codon,[g,c,t]).
get(codon,[g,g,a]).
get(codon,[g,g,c]).
get(codon,[g,g,g]).
get(codon,[g,g,t]).
get(codon,[g,t,a]).
get(codon,[g,t,c]).
get(codon,[g,t,g]).
get(codon,[g,t,t]).
get(codon,[t,a,a]).
get(codon,[t,a,c]).
get(codon,[t,a,g]).
get(codon,[t,a,t]).
get(codon,[t,c,a]).
get(codon,[t,c,c]).
get(codon,[t,c,g]).
get(codon,[t,c,t]).
get(codon,[t,g,a]).
get(codon,[t,g,c]).
get(codon,[t,g,g]).
get(codon,[t,g,t]).
get(codon,[t,t,a]).
get(codon,[t,t,c]).
get(codon,[t,t,g]).
get(codon,[t,t,t]).

get(amino_acid,a).
get(amino_acid,c).
get(amino_acid,d).
get(amino_acid,e).
get(amino_acid,f).
get(amino_acid,g).
get(amino_acid,h).
get(amino_acid,i).
get(amino_acid,k).
get(amino_acid,l).
get(amino_acid,m).
get(amino_acid,n).
get(amino_acid,p).
get(amino_acid,q).
get(amino_acid,r).
get(amino_acid,s).
get(amino_acid,t).
get(amino_acid,v).
get(amino_acid,w).
get(amino_acid,y).



% build_past

build_past(Order,[],_-_,_Data) :-
        Order>0,
        !,
        fail.
       

build_past(0,Data,_Past-[],Data) :-
        !.


build_past(N,[Elt|Rest],Past1-Past2,Rest_Data) :-
        Past2 = [Elt|Past3],
        N1 is N-1,
        build_past(N1,Rest,Past1-Past3,Rest_Data).
        




%--------
% Normalization predicates
% normalize(++Type,++Counting,--Probabilities)
%--------

normalize(Type,[],[]) :-
        member(Type,[nucleotide,codon,amino_acid]),
        !.
        
normalize(Type,[(Past,Countings)|Rest],[(Past,(Domain,Distribution))|Rest_Normalized]) :-
        member(Type,[nucleotide,codon,amino_acid]),
        !,
        normalize2(Countings,0,0,_Length_Final,_Sum_Final,Domain,Distribution),
        normalize(Type,Rest,Rest_Normalized).
        


normalize(length,Counting_List,[([],(Domain,Distribution))]) :-
        normalize2(Counting_List,0,0,_Length_Final,_Sum_Final,Domain,Distribution).


normalize2([],Length_Final,Sum_Final,Length_Final,Sum_Final,[],[]) :-
        !.

normalize2([(Elt,Count)|Rest_Counting],Length,Sum,Length_Final,Sum_Final,[Elt|Rest_Domain],[Proba|Rest_Count]) :-
        New_Length is Length+1,
        New_Sum is Sum+Count,
        normalize2(Rest_Counting,New_Length,New_Sum,Length_Final,Sum_Final,Rest_Domain,Rest_Count),
        (Sum_Final = 0 ->
            Proba is 1/Length_Final  % To avoid division by zero, a uniform distribution is generated when Sum = 0
        ;
            Proba is Count/Sum_Final
        ).



normalized_rec([],Normalized_Switches,Normalized_Switches) :-
        !.

normalized_rec([(Transition,Counter)|Rest],Normalized_Int,Normalized_Switches) :-
        !,
        Transition =.. [_,Begin,End],
        update_normalized(Normalized_Int,Begin,End,Counter,Normalized_Int1),
        normalized_rec(Rest,Normalized_Int1,Normalized_Switches).



update_normalized([],Begin,End,Counter,[(Begin,[End],[Counter])]) :-
        !.


update_normalized([(Begin,Rest_End,Rest_Counter)|Rest],Begin,End,Counter,[(Begin,[End|Rest_End],[Counter|Rest_Counter])|Rest]) :-
        !.

update_normalized([(B,B_End,B_Counter)|Rest],Begin,End,Counter,[(B,B_End,B_Counter)|Rest_Result]) :-
        !,
        update_normalized(Rest,Begin,End,Counter,Rest_Result).



normalized2([],[]) :-
        !.

normalized2([(Begin,Domain,Counters)|Rest],[(Begin,Domain,Distribution)|Rest_Result]) :-
        sum_list(Counters,Sum),
        normalized3(Sum,Counters,Distribution),
        normalized2(Rest,Rest_Result).



normalized3(_Sum,[],[]) :-
        !.

normalized3(Sum,[V|Rest],[Proba|Rest_Proba]) :-
        !,
        Proba is V/Sum,
        normalized3(Sum,Rest,Rest_Proba).




sum_list([],0) :-
        !.

sum_list([T|Rest],Sum) :-
        !,
        sum_list(Rest,Sum1),
        Sum is T+Sum1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% build_stat_facts(+Stats,-StatsFacts)
% builds a list of facts, one for each statistic
% e.g. 
% build_stat_facts(
%	[([a],[(a,0),(c,1),(g,0),(t,0)]),([c],[(a,0),(c,0),(g,0),(t,1)]),([g],[(a,0),(c,0),(g,1),(t,2)]),([t],[(a,1),(c,0),(g,2),(t,1)])],
%	[stat([a,a],0),stat([a,c],1),stat([a,g],0),stat([a,t],0),stat([c,a],0),stat([c,c],0),stat([c,g],0),
%	 stat([c,t],1),stat([g,a],0),stat([g,c],0),stat([g,g],1),stat([g,t],2),stat([t,a],1),stat([t,c],0),stat([t,g],2),stat([t,t],1)]
%	)


% Version for counts:
build_stat_facts([],[]).
build_stat_facts([(PastList,CountsList)|StatRest],Facts) :-
	build_stat_facts_single(PastList,CountsList,FactsList),
	build_stat_facts(StatRest,FactsListRest),
	append(FactsList,FactsListRest,Facts).

% Version for normalized stats:
build_stat_facts([(Past,(Present,Counts))|StatRest],Facts) :-
        build_stat_facts_combine_lists(Present,Counts,Combined),
        build_stat_facts([(Past,Combined)|StatRest],Facts).

build_stat_facts_single(_,[],[]).

build_stat_facts_single(PastList,[(N,Count)|Rest], [stat(Full,Count)|FactsRest]) :-
	append(PastList,[N],Full),
	build_stat_facts_single(PastList,Rest,FactsRest).

build_stat_facts_combine_lists([],[],[]).
build_stat_facts_combine_lists([A|Ar], [B|Br], [(A,B)|Cr]) :-
        build_stat_facts_combine_lists(Ar,Br,Cr).
        


% Normalize stat facts

normalize_stat_facts(Facts,NormalizedFacts) :-
        count_all(Facts,TotalCount),
        ((TotalCount == 0) ->
                NormalizedFacts = Facts
                ;
                normalize_stat_facts(Facts,TotalCount,NormalizedFacts)).


normalize_stat_facts([],_,[]).
normalize_stat_facts([stat(NGram,Count)|Rest],TotalCount,[stat(NGram,Norm)|NormRest]) :-
        Norm is Count / TotalCount,
        normalize_stat_facts(Rest,TotalCount,NormRest).

count_all([],0).
count_all([stat(_,Count)|Rest],TotalCount) :-
        count_all(Rest,CountRest),
        TotalCount is Count + CountRest.

