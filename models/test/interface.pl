:- task(test1([text,text],[opt1(a),opt2(b)],text)).
:- task(has_no_implementation([text,text],[opt1(a),opt2(b)],text)).

:- task(multifile([text],[],[text,text])).

test1([I1,I2],Opts,Out) :-
	writeln('dummy implementation of test1').
	

% A task with multiple output files
multifile([InputFile],Options,[OutputFile1,OutputFile2]) :-
	copy_file(InputFile,OutputFile1),
	copy_file(InputFile,OutputFile2).