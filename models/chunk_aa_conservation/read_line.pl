%
% readline.pl
% 14.10.2009
% Ole Torp Lassen
%============================================================================
% read_line/2, read_tab/2, read_token/2
%----------------------------------------------------------------------------
% reads in a line or a tab or a token with no spaces of data from specified input-stream
% and represents it as a list of character codes..
%============================================================================
read_line(File,CodeList):-
	(
	at_end_of_stream(File),!,
	CodeList = [eof]
	;
	get_code(File,Code),
	(
	member(Code,[10]),!,
	CodeList=[]
	;
	read_rest_of_line(File,List),
	CodeList = [Code|List]
	)
	)
	.

read_rest_of_line(File,CodeList):-
	(
	at_end_of_stream(File),!,
	CodeList = []
	;
	get_code(File,Code),
	(
	member(Code,[10]),!,
	CodeList=[]
	;
	read_rest_of_line(File,RestOfCodes),
	CodeList=[Code|RestOfCodes]
	)
	).
%========================================================================
read_tab(File,CodeList):-
	get_code(File,Code),
	Code = -1 -> CodeList = [eof]
	;
	(
	read_rest_of_tab(File,RestOfCodes),
	CodeList=[Code|RestOfCodes]
	).

read_rest_of_tab(File,CodeList):-
	get_code(File,Code),
	Code = -1 -> CodeList = [eof]
	;
	(
	member(Code,[9,10]),!,
	CodeList=[]
	;
	read_rest_of_tab(File,RestOfCodes),
	CodeList=[Code|RestOfCodes]
	).
%=======================================================================
% read_token/3
% reads next nonspace-initiated token, terminated by a some whitespace
% counts number of spaces encounterd before first nonspace
%

read_token(File,N_spaces,CodeList):-
	next_nonspace(File,N_spaces,Code),
	(
	Code = -1 -> CodeList = [eof]
	;
	(
	member(Code,[9,10,32,13]),!,
	CodeList=[]
	;
	read_rest_of_token(File,RestOfCodes),
	CodeList=[Code|RestOfCodes]
	)
	).

read_rest_of_token(File,CodeList):-

	get_code(File,Code),
	(
	Code = -1 -> CodeList = [eof]
	;
	(
	member(Code,[9,10,32,13]),!,
	CodeList=[]
	;
	read_rest_of_token(File,RestOfCodes),
	CodeList=[Code|RestOfCodes]
	)
	).

next_nonspace(File,N,Code):-
	get_code(File,C),
	(
	C = 32 -> next_nonspace(File,M,Code),	N is M+1
	;
	Code = C,	N is 0
	).

read_n_spaces(_,0).
read_n_spaces(File,N):-
	get_code(File,32),
	M is N-1,
	read_n_spaces(File,M).
