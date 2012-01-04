:- use(script).
:- use(prologdb).

sequence1([a,g,c,t,a,g,c,t]).

file <- sequence1(Seq) | file::from_sequence([],[sequence(Seq)]).

testcase(from_sequence) :-
	get_result_file(file,File),
	file_exists(File),
	terms_from_file(File,Terms),
	sequence1(Seq1),
	length(Seq1,S1L),
	member(sequence(_Id,1,S1L,Seq1),Terms).


% Testing 'getting' of local files:

local_file <- tell('/tmp/localfile'), writeln('test(42).'), told | file::get('file:///tmp/localfile').

testcase(get_local) :-
	get_result_file(local_file,File),
	file_exists(File),
	terms_from_file(File,[test(42)]).
	
	
% Testing 'getting' via ftp

ftp_file <- file::get('ftp://ftp.dkuug.dk/pub/HEADER').

testcase(get_ftp) :-
	get_result_file(ftp_file,File),
	file_exists(File).
	

% Testing 'getting' via http

http_file <- file::get('http://ftp.dkuug.dk/pub/HEADER').

testcase(get_http) :-
	get_result_file(http_file,File),
	file_exists(File).

	
	
	
	
	

