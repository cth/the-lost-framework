get_list_codonsTREE2ANNOT(A, _) :-
        A=[[a,a,a],[a,a,t],[a,a,c],[a,a,g],[a,t,a],[a,t,t],[a,t,c],[a,t,g],[a,c,a],[a,c,t],[a,c,c],[a,c,g],[a,g,a],[a,g,t],[a,g,c],[a,g,g],[t,a,t],[t,a,c],[t,t,a],[t,t,t],[t,t,c],[t,t,g],[t,c,a],[t,c,t],[t,c,c],[t,c,g],[t,g,t],[t,g,c],[t,g,g],[c,a,a],[c,a,t],[c,a,c],[c,a,g],[c,t,a],[c,t,t],[c,t,c],[c,t,g],[c,c,a],[c,c,t],[c,c,c],[c,c,g],[c,g,a],[c,g,t],[c,g,c],[c,g,g],[g,a,a],[g,a,t],[g,a,c],[g,a,g],[g,t,a],[g,t,t],[g,t,c],[g,t,g],[g,c,a],[g,c,t],[g,c,c],[g,c,g],[g,g,a],[g,g,t],[g,g,c],[g,g,g]].

hmm_lengthTREE2ANNOT(G, H, I, A) :-
        split_viterbit_tree_into_parts(A, D, E, F),
        D=hmm_length(G,H),
        global_store_for_list_of_calls(B, E),
        global_store_for_list_of_calls(C, F),
        next_call_from_global_store(C, msw(begin,J)),
        set_paramsTREE2ANNOT(H, _),
        write(test),
        nl, !,
        next_call_from_global_store(B, K),
        hmm_length_recTREE2ANNOT(J, H, _, G, I, K),
        clean_global_store_for_list_of_calls(B),
        clean_global_store_for_list_of_calls(C).

hmm_length_recTREE2ANNOT(G, H, I, [], [], A) :-
        split_viterbit_tree_into_parts(A, D, E, F),
        D=hmm_length_rec(G,H,I,[]),
        global_store_for_list_of_calls(B, E),
        global_store_for_list_of_calls(C, F),
        G=end, !,
        clean_global_store_for_list_of_calls(B),
        clean_global_store_for_list_of_calls(C).

hmm_length_recTREE2ANNOT(G, H, I, [J,K,L], [1,1,1], A) :-
        split_viterbit_tree_into_parts(A, D, E, F),
        D=hmm_length_rec(G,H,I,[J,K,L]),
        global_store_for_list_of_calls(B, E),
        global_store_for_list_of_calls(C, F),
        G=stop, !,
        next_call_from_global_store(C, msw(emit(G),[J,K,L])),
        next_call_from_global_store(C, msw(trans(G),M)),
        next_call_from_global_store(B, N),
        hmm_length_recTREE2ANNOT(M, H, I, [], [], N),
        clean_global_store_for_list_of_calls(B),
        clean_global_store_for_list_of_calls(C).

hmm_length_recTREE2ANNOT(G, H, I, [J,K,L|M], [1,1,1|N], A) :-
        split_viterbit_tree_into_parts(A, D, E, F),
        D=hmm_length_rec(G,H,I,[J,K,L|M]),
        global_store_for_list_of_calls(B, E),
        global_store_for_list_of_calls(C, F),
        G=start, !,
        next_call_from_global_store(C, msw(emit(G),[J,K,L])),
        next_call_from_global_store(C, msw(duration(H),I)),
        O is I-1,
        next_call_from_global_store(C, msw(trans(G),P)),
        next_call_from_global_store(B, Q),
        hmm_length_recTREE2ANNOT(P, H, O, M, N, Q),
        clean_global_store_for_list_of_calls(B),
        clean_global_store_for_list_of_calls(C).

hmm_length_recTREE2ANNOT(G, H, I, [J,K,L|M], [1,1,1|N], A) :-
        split_viterbit_tree_into_parts(A, D, E, F),
        D=hmm_length_rec(G,H,I,[J,K,L|M]),
        global_store_for_list_of_calls(B, E),
        global_store_for_list_of_calls(C, F),
        G=c, !,
        next_call_from_global_store(C, msw(emit(G),[J,K,L])),
        next_call_from_global_store(C, msw(trans(G,I),O)),
        P is I-1,
        next_call_from_global_store(B, Q),
        hmm_length_recTREE2ANNOT(O, H, P, M, N, Q),
        clean_global_store_for_list_of_calls(B),
        clean_global_store_for_list_of_calls(C).

hmm_length_recTREE2ANNOT(G, H, [I|J], [0|K], A) :-
        split_viterbit_tree_into_parts(A, D, E, F),
        D=hmm_length_rec(G,H,[I|J]),
        global_store_for_list_of_calls(B, E),
        global_store_for_list_of_calls(C, F),
        G=nc, !,
        next_call_from_global_store(C, msw(emit(G),I)),
        next_call_from_global_store(C, msw(trans(G),L)),
        next_call_from_global_store(B, M),
        hmm_length_recTREE2ANNOT(L, H, J, K, M),
        clean_global_store_for_list_of_calls(B),
        clean_global_store_for_list_of_calls(C).

get_epsilonTREE2ANNOT(-0.0, _).

set_paramsTREE2ANNOT(A, _) :-
        compute_domain_smoothTREE2ANNOT(A, B, _),
        normalize(length, B, [([],_,C)]),
        fix_sw(duration(A), C).

compute_domainTREE2ANNOT(A, B, _) :-
        stats_length(A, C),
        compute_domain_recTREE2ANNOT(C, 1, B, _).

compute_domain_recTREE2ANNOT([], _, [], _) :- !.

compute_domain_recTREE2ANNOT([(A,B)|C], D, [(E,B)|F], _) :-
        E is A//3,
        D=E, !,
        G is D+1,
        compute_domain_recTREE2ANNOT(C, G, F, _).

compute_domain_recTREE2ANNOT([(A,B)|C], D, [(D,E)|F], _) :- !,
        get_epsilonTREE2ANNOT(E, _),
        G is D+1,
        compute_domain_recTREE2ANNOT([(A,B)|C], G, F, _).

compute_domain_smoothTREE2ANNOT(A, B, _) :-
        stats_length(A, []), !,
        B=[].

compute_domain_smoothTREE2ANNOT(range([A,B]), [(C,D)|E], _) :- !,
        stats_length(range([A,B]), [(F,G)|H]),
        C is ceiling(A/3),
        I is F//3,
        (   C==I ->
            G=D,
            J=[C,G],
            write(get_right(H,K)),
            nl,
            get_rightTREE2ANNOT(H, K, _),
            write(K),
            nl,
            L=H
        ;   L=[(F,G)|H],
            D=0,
            J=[C,D],
            K=[I,G]
        ),
        M is C+1,
        write(J),
        write('-'),
        write(K),
        nl,
        write(L),
        nl,
        compute_domain_smooth_recTREE2ANNOT(L, M, J, K, E, _).

compute_domain_smoothTREE2ANNOT(A, [(1,B)|C], _) :-
        stats_length(A, [(D,E)|F]),
        G is D//3,
        (   G==1 ->
            E=B,
            H=[1,E],
            get_rightTREE2ANNOT(F, I, _),
            J=F
        ;   J=[(D,E)|F],
            B=0,
            H=[1,B],
            I=[G,E]
        ),
        compute_domain_smooth_recTREE2ANNOT(J, 2, H, I, C, _).

compute_domain_smooth_recTREE2ANNOT([], _, _, _, [], _) :- !.

compute_domain_smooth_recTREE2ANNOT([(A,B)|C], D, _, E, [(F,B)|G], _) :-
        F is A//3,
        F==D, !,
        H is D+1,
        get_rightTREE2ANNOT(C, I, _),
        write(E),
        write('-'),
        write(I),
        nl,
        compute_domain_smooth_recTREE2ANNOT(C, H, E, I, G, _).

compute_domain_smooth_recTREE2ANNOT(A, B, [C,D], [E,F], [(B,G)|H], _) :-
        I is C-E,
        write(I),
        nl,
        G is(D-F)/I*B+(C*F-E*D)/I,
        J is B+1,
        compute_domain_smooth_recTREE2ANNOT(A, J, [C,D], [E,F], H, _).

get_rightTREE2ANNOT([], _, _) :- !.

get_rightTREE2ANNOT([(A,B)|_], [C,B], _) :- !,
        C is A//3.


