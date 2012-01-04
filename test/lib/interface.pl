:- use(interface).
:- use(script).

testcase(task_declaration) :-
	task_declaration(test,test1,test1([text,text],[opt1(a),opt2(b)],text)).

testcase(declared_model_options) :-
	declared_model_options(test,test1,[opt1(a),opt2(b)]).
	
testcase(declared_input_formats) :-
	declared_input_formats(test,test1,[text,text]).
	
testcase(declared_output_format) :-
	declared_output_format(test,test1,text).
	
testcase(task_has_implementation_1) :-
	task_has_implementation(test,test1).

testcase(task_has_implementation_2) :-
	not(task_has_implementation(test,test2)).
	
testcase(check_valid_model_call) :-
	catch(check_valid_model_call(test,test1,[blah,blah],[unknown(1)]),error(interface(model_called_with_undeclared_options(test,[unknown(1)]))),true).
	
testcase(expand_model_options_1) :-
	expand_model_options(test,test1,[],[opt1(a),opt2(b)]).

testcase(expand_model_options_2) :-
	expand_model_options(test,test1,[opt1(c)],[opt1(c),opt2(b)]).
	

