:- use(script).

sequence1([a,g,c,t,a,g,c,t]).

% Test multi file goals

sample_file <- sequence1(S) | file::from_sequence([],[sequence(S)]).

goal1, goal2 <- test::multi(sample_file).

testcase(very_simple_run) :-
	run(sample_file),
	goal_result_file(sample_file,File),
	file_exists(File).

testcase(test_multi_file_goals) :-
	rerun_recursive(goal1),
	goal_result_file(goal1,Goal1File),
	goal_result_file(goal2,Goal2File),
	sequence1(Seq1),	
	terms_from_file(Goal1File,[sequence(_Id,1,S1L,Seq1)]),
	terms_from_file(Goal2File,[sequence(_Id,1,S1L,Seq1)]).
	
testcase(match_target_rule) :-
		match_target_rule(sample_file,_R,1),
		match_target_rule(goal1,R,1),
		match_target_rule(goal2,R,2).
	