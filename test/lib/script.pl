:- use(script).

sequence1([a,g,c,t,a,g,c,t]).

% Test multi file goals

sample_file <- sequence1(S) | file::from_sequence([],[sequence(S)]).

goal1, goal2 <- test::multifile(sample_file).

single_input_file_test <- test::one_input(sample_file).

sample_file(X) <- sequence1(S) | file::from_sequence([],[sequence(S)]).

append_file <- append_all((X in [1,2], labeling(X)), sample_file(X)).

testcase(very_simple_run) :-
	rerun_recursive(sample_file),
	get_result_file(sample_file,File),
	file_exists(File).

testcase(test_multi_file_goals) :-
	rerun_recursive(goal1),
	get_result_file(goal1,Goal1File),
	get_result_file(goal2,Goal2File),
	sequence1(Seq1),	
	terms_from_file(Goal1File,[sequence(_Id,1,S1L,Seq1)]),
	terms_from_file(Goal2File,[sequence(_Id,1,S1L,Seq1)]).
	
testcase(append_all) :-
	rerun_recursive(append_file),
	get_result_file(append_file,F),
	terms_from_file(F,Terms),
	append(Same,Same,Terms).

/* Not even sure I want this to work!
testcase(single_input_file) :-
	rerun_recursive(single_input_file_test),
	get_result_file(single_input_file_test,OneFile),
	sequence1(Seq1),
	terms_from_file(OneFile,[sequence(_Id,1,S1L,Seq1)]).
*/

testcase(match_target_rule) :-
		match_target_rule(sample_file,_R,1),
		match_target_rule(goal1,R,1),
		match_target_rule(goal2,R,2).
