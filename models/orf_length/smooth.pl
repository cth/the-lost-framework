% To be tested and debugged..

first_n(_,[],[]).
first_n(0,_,[]).
first_n(N,[E|R1],[E|R2]) :-
	N1 is N - 1,
	first_n(N1,R1,R2).
	
last_n(N,L,LastN) :-
	reverse(L,LR),
	first_n(N,LR,ReverseLastN),
	reverse(ReverseLastN,LastN).
	
%%
% smooth_counts(Domain,Counts)

split_kernel(Kernel,Center,LeftSide,RightSide) :-
	length(Kernel,KernelSize),
	FlankSize is KernelSize // 2,
	FlankSizePlusOne is FlankSize + 1,
	first_n(FlankSize,Kernel,LeftSide),
	last_n(FlankSizePlusOne,Kernel,[Center|RightSide]).
	
multiply_lists([],[],[]).
multiply_lists([A|As],[B|Bs],[R|Rs]) :-
	R is A * B,
	multiply_lists(As,Bs,Rs).

apply_smoothing_kernel(Kernel,Count,LeftCounts,RightCounts,NewCount) :-
	write('.'),
	length(Kernel,KernelSize),
	FlankSize is KernelSize // 2,
	first_n(FlankSize,RightCounts,RelevantRightCounts),
	last_n(FlankSize,LeftCounts,RelevantLeftCounts),
	length(RelevantRightCounts,RightLen),
	length(RelevantLeftCounts,LeftLen),
	TotalLength is 1 + RightLen + LeftLen,
	split_kernel(Kernel,Center,KernelLeft,KernelRight),
	first_n(LeftLen,KernelLeft,RelevantKernelLeft),
	last_n(RightLen,KernelRight,RelevantKernelRight),
	multiply_lists(RelevantKernelLeft,RelevantLeftCounts,ResultListLeft),
	multiply_lists(RelevantKernelRight,RelevantRightCounts,ResultListRight),
	sumlist(ResultListLeft,WeightedSumLeft),
	sumlist(ResultListRight,WeightedSumRight),
	NewCount is (Count * Center + WeightedSumLeft + WeightedSumRight) / TotalLength.
	
smooth_counts(Kernel,Counts,SmoothedCounts) :-
		smooth_counts(Kernel,[],Counts,SmoothedCounts).
	
smooth_counts(_Kernel,_LeftCounts,[],[]).
		
smooth_counts(Kernel,LeftCounts,[CurrentCount|RightCounts],[SmoothedCount|SmoothedCountsRest]) :-
	apply_smoothing_kernel(Kernel,CurrentCount,LeftCounts,RightCounts,SmoothedCount),
	smooth_counts(Kernel,[CurrentCount|LeftCounts],RightCounts,SmoothedCountsRest).
	
test :-
	Kernel = [ 0.1, 2, 0.1 ],
%	Kernel = [ 0.1,0.8,0.1 ],
%	Counts = [ 1, 0, 1 , 0, 1,0,1, 2, 4, 2, 1, 10, 1 ],
	Counts = [ 10, 0,0,0,0,0,0,0, 10, 0, 10, 0, 10],
	smooth_counts(Kernel,Counts,SmoothedCounts),
	write(SmoothedCounts),nl,
	sumlist(SmoothedCounts,Total),
	write(Total).
	
smooth_msw(0,_SwitchName,_Kernel).
	
smooth_msw(Iterations,SwitchName,Kernel) :-
	!,
	write(smooth_msw(Iterations,SwitchName,Kernel)),nl,
	Iterations1 is Iterations - 1,
	values(SwitchName,Outcomes),
	atom_concat('msw_',SwitchName,SwitchGoalName),
	findall(P,(member(O,Outcomes),SwitchGoal=..[SwitchGoalName,O],prob(SwitchGoal,P),write(prob(SwitchGoal,P)),nl),Probs),
	write('before smooth;'),
	tell('/tmp/before.dat'),
	forall(member(P,Probs),(write(P),nl)),
	told,
	write(Probs),nl,
	smooth_counts(Kernel,Probs,SmoothCounts),
	write('after smooth:'),
	tell('/tmp/after.dat'),
	forall(member(P,SmoothCounts),(write(P),nl)),
	told,
	write(SmoothCounts),nl,
	set_switch_from_counts(SmoothCounts,SwitchName),
	smooth_msw(Iterations1,SwitchName,Kernel).

%% set_switches_from_couns(+Switch)
% Set the probability for outcome from Switch to the normalized value
% of the observed counts for the switch
set_switch_from_counts(CountList,Switch) :-
	sumlist(CountList,Total),
    compute_frequencies_from_counts(Total,CountList,FreqList),
    write(set_sw(Switch,FreqList)),nl,
    set_sw(Switch,FreqList).

%% compute_frequencies_from_counts(+Total,+CountList,+FreqList)
% Compute the relative frequency of each outcome
compute_frequencies_from_counts(_,[],[]).
compute_frequencies_from_counts(Total,[C|Cs],[F|Fs]) :-
	F is C / Total,
	compute_frequencies_from_counts(Total,Cs,Fs).

