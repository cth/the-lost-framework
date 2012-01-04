:- task(test1([text,text],[opt1(a),opt2(b)],text)).
:- task(has_no_implementation([text,text],[opt1(a),opt2(b)],text)).

test1([I1,I2],Opts,Out) :-
	writeln('dummy implementation of test1').