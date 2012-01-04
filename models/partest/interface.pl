
top(In,Opt,Out) :- go(In,Opt,Out).
mid(In,Opt,Out) :- go(In,Opt,Out).
bottom(In,Opt,Out) :- go(In,Opt,Out).

go(_,_,OutputFile) :-
	writeln('starting'),
	sleep(10000),
	writeln('stopping'),
	tell(OutputFile),
	writeln('Hello world'),
	told.
