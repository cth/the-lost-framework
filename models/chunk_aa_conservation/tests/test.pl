:- ['../../lost.pl'].

:- lost_include_api(utils_parser_report).

test :-
	open('test.pl',read,S),
	do_readline(S),
	close(S).
	
do_readline(S) :-
	readline(S,Symbols),
	write(Symbols),nl,
	(Symbols == [ eof ] ->
		true
		;
		do_readline(S)).