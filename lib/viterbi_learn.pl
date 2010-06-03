:- ['../../lost.pl'].
:- lost_include_api(io).

erf_learn_file(File) :-
	clear_counters,
	add_pseudo_counts,
        open(File,read,Stream),
        erf_learn_stream(Stream),
        close(Stream),
	set_switches_from_counts.

erf_learn_file_count_only(File) :-
	clear_counters,	
        open(File,read,Stream),
        erf_learn_stream(Stream),
        close(Stream).

erf_learn_stream(Stream) :-
        read(Stream,Term),
        ((Term == end_of_file) ->
                true
                ;
                erf_learn_term(Term),
                !,
                erf_learn_stream(Stream)).

erf_learn([]).
erf_learn([G|Gs]) :-
	clear_counters,
        erf_learn_term(G),
        !,
        erf_learn(Gs).

erf_learn_term(G) :-
        viterbit(G,_,T),
        tree_switches(T,S),
        forall(member(M,S),count_msw(M)),
	write('.'),flush_output.

%%%% 

count_msw(MSW) :-
        retract(count_table(MSW,OldCount)),
	!,
        NewCount is OldCount + 1,
        assert(count_table(MSW,NewCount)).

count_msw(MSW) :-
	assert(count_table(MSW,1)).

clear_counters :-
	retractall(count_table(_,_)),
	retractall(freq_table(_,_)).

merge_counts_files([]).
merge_counts_files([CountsFile|CFs]) :-
        load_counts_file(CountsFile),
        merge_counts_files(CFs).

load_counts_file(CountsFile) :-
        terms_from_file(CountsFile,CountTerms),
        forall(member(Term,CountTerms),merge_count_term(Term)).

save_counts_file(CountsFile) :-
	write('save_counts_to_file: '),write(CountsFile), nl,
        findall(count_table(A,B),count_table(A,B),CountTerms),
        terms_to_file(CountsFile,CountTerms).
        
% Merge the counts for an existing/asserted count/2 fact with the
% counts given in CountTerm
merge_count_term(count_table(msw(MSW,Outcome),Count)) :-
        retract(count_table(msw(MSW,Outcome),LocalCount)),
	!,
        NewCount is LocalCount + Count,
        assert(count_table(msw(MSW,Outcome),NewCount)).
merge_count_term(T) :-
	assert(T).

add_pseudo_counts :-
	findall(msw(V,O), (values(V,Os),member(O,Os)), MSWs),
	forall(member(MSW,MSWs), assert(count_table(MSW,0.00001))).

set_switches_from_counts :-
	findall(V,values(V,_),AllVarsWithDups),
	eliminate_duplicate(AllVarsWithDups,AllVars),
	forall(member(V,AllVars),set_switches_from_counts(V)).

set_switches_from_counts(V) :-
	values(V,Outcomes),
	count_list_from_outcome_list(V,Outcomes,CountList),
	sumlist(CountList,Total),
	compute_frequencies_from_counts(Total,CountList,FreqList),
	write(set_sw_all(V,FreqList)),nl,
	set_sw_all(V,FreqList).

count_list_from_outcome_list(_,[],[]).
count_list_from_outcome_list(Var,[O|Os],[C|Cs]) :-
	count_table(msw(Var,O),C),
	count_list_from_outcome_list(Var,Os,Cs).

compute_frequencies_from_counts(_,[],[]).
compute_frequencies_from_counts(Total,[C|Cs],[F|Fs]) :-
	F is C / Total,
	compute_frequencies_from_counts(Total,Cs,Fs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The count_table keeps track of each used msw. Each time an msw
% is use, the entry in count_table is incremented
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
count_table_as_list(List) :-
	findall([MSW,Count], count_table(MSW,Count), List).

count_table_variables(Vars) :-
	findall(Var,count_table(msw(Var,_),_),VarsDup),
	eliminate_duplicate(VarsDup,Vars).

count_table_total_count_for_variable(Variable, Total) :-
	findall(Count,count_table(msw(Variable,_),Count),Counts),
	sumlist(Counts,Total).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The frequency table is computed from the count table
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Create a list of switches used in a viterbi tree 
tree_switches([],[]) :- !.
tree_switches(msw(Var,Outcome),[msw(Var,Outcome)]) :- !.
tree_switches([Goal|Gs],Msws) :-
        !,
        tree_switches(Goal,Msws1),
        tree_switches(Gs,Msws2),
        append(Msws1,Msws2,Msws).

tree_switches(_,[]).
