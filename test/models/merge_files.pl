:- use(script).
:- use(prologdb).

sequence1([a,g,c,t,a,g,c,t]).
sequence2([a,a,a,a]).

file1 <- sequence1(Seq) | file::from_sequence([],[sequence(Seq)]).
file2 <- sequence2(Seq) | file::from_sequence([],[sequence(Seq)]).

merged_files <- merge_files::merge(file1,file2).

testcase(merge_files) :-
	rerun_recursive(merged_files),
	get_result_file(merge_files,File),
	file_exists(File),
	terms_from_file(File,Terms),
	sequence1(Seq1),
	length(Seq1,S1L),
	sequence2(Seq2),
	length(Seq2,S2L),
	length(Terms,2), % exactly two terms should be present in results file
	member(sequence(_Id,1,S1L,Seq1),Terms),
	member(sequence(_Id,1,S2L,Seq2),Terms).
	

