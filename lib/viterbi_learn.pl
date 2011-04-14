:- lost_include_api(misc_utils).
:- lost_include_api(io).

%% viterbi_learn_file(+File)
% Learns the parameters of a PRISM program from the observed goals 
% supplied in File
viterbi_learn_file(File) :-
	clear_counters,
	add_pseudo_counts,
        open(File,read,Stream),
        viterbi_learn_stream(Stream,0),
        close(Stream),
	set_switches_from_counts.

%% viterbi_learn_file_count_only(+File)
% Does empirical frequency counting of the MSWs involved
% in the viterbi path of the observed goals in File 
viterbi_learn_file_count_only(File) :-
	clear_counters,	
        open(File,read,Stream),
        viterbi_learn_stream(Stream),
        close(Stream).

%% viterbi_learn_stream(+Stream)
% Does empirical frequency counting of the MSWs involved
% in the viterbi path of the observed goals in Stream 
viterbi_learn_stream(Stream,NumGoals) :-
        read(Stream,Term),
        ((Term == end_of_file) ->
                true
                ;
                viterbi_learn_term(Term),
                ReportNum is NumGoals mod 100,
                ReportDot is NumGoals mod 10,
                ((ReportNum == 0) ->
                        write(NumGoals)
                        ;
                        ((ReportDot == 0) -> write('.');true)),
                flush_output,
                NewNumGoals is NumGoals + 1,
                !,
                viterbi_learn_stream(Stream,NewNumGoals)).

%% viterbi_learn(+ListOfGoals)
% Does empirical frequency counting of the MSWs involved
% in the viterbi path of the observed goals in ListOfGoals 
viterbi_learn([]).
viterbi_learn([G|Gs]) :-
	clear_counters,
        viterbi_learn_term(G),
        table_remove0,
        !,
        viterbi_learn(Gs).

%% viterbi_learn_term(+G)
% Does empirical frequency counting of the MSWs involved
% in the viterbi path of the observed goal G
viterbi_learn_term(G) :-
        viterbit(G,_,T),
        tree_switches(T,S),
        forall(member(M,S),count_msw(M)).

%%%% 

%%  count_msw(+MSW)
% increment the counter for MSW 
count_msw(MSW) :-
        retract(count_table(MSW,OldCount)),
	!,
        NewCount is OldCount + 1,
        assert(count_table(MSW,NewCount)).

count_msw(MSW) :-
	assert(count_table(MSW,1)).

%% clear_counters
% reset all MSW counts to zero 
clear_counters :-
	retractall(count_table(_,_)),
	retractall(freq_table(_,_)).

%% merge_counts_files(+FileList)
% Takes a FileList and reads MSW counts from each of the files
% in the list. The counts read are added to the allready existing
% counts, effectively summing the counts of all the files.
merge_counts_files([]).
merge_counts_files([CountsFile|CFs]) :-
        load_counts_file(CountsFile),
        merge_counts_files(CFs).

%% load_counts_file(+CountsFile)
%  
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

%% add_pseudo_counts
% Add a small pseudo count for each possible mws outcome
add_pseudo_counts :-
%	% PRISM2 version:
%	findall(msw(V,O), (get_values(V,Os),member(O,Os)), MSWs),
	findall(msw(V,O), (values(V,Os),member(O,Os)), MSWs),
	forall(member(MSW,MSWs), assert(count_table(MSW,0.00001))).

%% set_switches_from_counts
% Set the probabilities of all switches using recorded counts
set_switches_from_counts :-
	findall(V,values(V,_),AllVarsWithDups),
	eliminate_duplicate(AllVarsWithDups,AllVars),
	nl,
	forall(member(V,AllVars),set_switches_from_counts(V)).

%% set_switches_from_couns(+Switch)
% Set the probability for outcome from Switch to the normalized value
% of the observed counts for the switch
set_switches_from_counts(Switch) :-
	values(Switch,Outcomes),
	count_list_from_outcome_list(Switch,Outcomes,CountList),
	sumlist(CountList,Total),
	compute_frequencies_from_counts(Total,CountList,FreqList),
	write(set_sw_all(Switch,FreqList)),nl,
	set_sw_all(Switch,FreqList).


%% count_list_from_outcome_list(+Switch,+OutcomesList,-CountList)
% produce a list of counts for outcome of Switch 
count_list_from_outcome_list(_,[],[]).
count_list_from_outcome_list(Switch,[O|Os],[C|Cs]) :-
	count_table(msw(Switch,O),C),
	count_list_from_outcome_list(Switch,Os,Cs).

%% compute_frequencies_from_counts(+Total,+CountList,+FreqList)
% Compute the relative frequency of each outcome 
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

% Create a list of switches used in a viterbi tree 
tree_switches([],[]) :- !.
tree_switches(msw(Var,Outcome),[msw(Var,Outcome)]) :- !.
tree_switches([Goal|Gs],Msws) :-
        !,
        tree_switches(Goal,Msws1),
        tree_switches(Gs,Msws2),
        append(Msws1,Msws2,Msws).

tree_switches(_,[]).

