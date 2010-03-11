heinTREE2ANNOT(G, H, A) :-
        split_viterbit_tree_into_parts(A, D, E, F),
        D=hein(G),
        global_store_for_list_of_calls(B, E),
        global_store_for_list_of_calls(C, F),
        next_call_from_global_store(C, msw(begin,I)),
        next_call_from_global_store(B, J),
        hein_recTREE2ANNOT(I, [], G, H, J),
        clean_global_store_for_list_of_calls(B),
        clean_global_store_for_list_of_calls(C).

hein_recTREE2ANNOT(end, G, [], [end], A) :-
        split_viterbit_tree_into_parts(A, D, E, F),
        D=hein_rec(end,G,[]),
        global_store_for_list_of_calls(B, E),
        global_store_for_list_of_calls(C, F), !,
        clean_global_store_for_list_of_calls(B),
        clean_global_store_for_list_of_calls(C).

hein_recTREE2ANNOT(G, H, [I|J], [G|K], A) :-
        split_viterbit_tree_into_parts(A, D, E, F),
        D=hein_rec(G,H,[I|J]),
        global_store_for_list_of_calls(B, E),
        global_store_for_list_of_calls(C, F),
        G\=end,
        build_pastTREE2ANNOT(G, H, L, _),
        next_call_from_global_store(C, msw(emit(G,L),I)),
        (   H=[_,M,N] ->
            O=[M,N,I]
        ;   append(H, [I], O)
        ),
        start_or_stopTREE2ANNOT(G, O, P, _),
        next_call_from_global_store(C, msw(trans(G,P),Q)),
        next_call_from_global_store(B, R),
        hein_recTREE2ANNOT(Q, O, J, K, R),
        clean_global_store_for_list_of_calls(B),
        clean_global_store_for_list_of_calls(C).

start_codonTREE2ANNOT([[a,t,g]], _).

stop_codonTREE2ANNOT([[t,a,a],[t,g,a],[t,a,g]], _).

start_or_stopTREE2ANNOT(A, B, C, _) :-
        member(A, [nc,s1,s2,d12]), !,
        start_codonTREE2ANNOT(D, _),
        (   member(B, D) ->
            C=start
        ;   C=no_start
        ).

start_or_stopTREE2ANNOT(A, B, C, _) :-
        member(A, [s3,d31,d23,t123]), !,
        stop_codonTREE2ANNOT(D, _),
        (   member(B, D) ->
            C=stop
        ;   C=no_stop
        ).

build_pastTREE2ANNOT(nc, _, [], _) :- !.

build_pastTREE2ANNOT(s1, _, [], _) :- !.

build_pastTREE2ANNOT(s2, [_,_,A], [A], _) :- !.

build_pastTREE2ANNOT(d12, [_,_,A], [A], _) :- !.

build_pastTREE2ANNOT(_, [_,A,B], [A,B], _) :- !.

smswTREE2ANNOT(A, B, _) :-
        values(A, [B|_]).


