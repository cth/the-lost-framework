% The idea is to formulate a declarative (non-procedural way of specifying models)
:- op(1200,xfx,'<-'). % same precendece as :-
:- op(1050,xfx,'::').
:- op(1100,xfx,'|').

% test
%model1(test) <- model2::task1([arg1,arg2],[option(1),option(2)]).

%accuracy(X) <- accuracy::measure_accuracy([genbank,X]).

list_lost_rules :-
	listing('<-'/2).
	
show_lost_rules :-
	clause('<-'(Output,Y),Body),
	nl,
	write('Processing: '),
	portray_clause('<-'(Output,Y)),
	Y =.. [ '::', Model, TaskSpecification ],
	write('output name: '), write(Output),nl,
	write('model: '), write(Model),nl,
	write('Task spec: '), write(TaskSpecification), nl,
	writeln(Body),
	fail.
show_lost_rules.
		
run(Output) :-
	write('run:'), write(Output),nl,
	run(Output,File),
	write('Success '),
	write(Output),
	write(' ==> '),
	write(File),
	nl.
	
run(lost_data_file(Identifier),File) :-
	atom(Identifier),
	lost_data_file(Identifier,File).

run(file(File),File) :-
	atom(File),
	% Check that file exists
	(file_exists(File) ->
		true
		;
		atom_concat_list(['File ', File, ' does not exist!'],ErrMsg),
		throw(ErrMsg)).

% Run with procotol identifiers
run(Target,File) :-
	atom(Target),
	atom_codes(Target,TargetSyms),
	atom_codes('file://', MatchSyms),
	append(MatchSyms,FileCodes,TargetSyms),
	atom_codes(File,FileCodes).

run(Target,Target) :-
	atom(Target),
	atom_codes(Target,TargetSyms),
	map(atom_codes,['ftp://','http://'],Matchers),
	member(MatchSyms,Matchers),
	atom_codes('ftp://',MatchSyms),
	append(MatchSyms,_,TargetSyms).
		
run(Output,_File) :-
	writeln(run(Output,file)),
	fail.

run(Output,File) :-
	clause('<-'(Output,Rule),true),
	write(here),
	parse_guard_and_body(Rule,Guard,Model,TaskSpec),
	call(Guard),
	parse_task_specification(TaskSpec,Task,Inputs,Options),
	findall(DependencyFile,(member(Dependency,Inputs),run(Dependency,DependencyFile)), InputFiles),
	RealTaskSpec =.. [ Task, InputFiles, Options, File ],
	run_model(Model,RealTaskSpec).

	
parse_guard_and_body(Spec, true, Model, TaskSpec) :-
	write(Spec),nl,
	Spec =.. [ '::', Model, TaskSpec].
	
parse_guard_and_body(Spec, Guard, Model, TaskSpec) :-
	Spec =.. [ '|', Guard, Body ],
	Body =.. [ '::', Model, TaskSpec].
	
%%
% parse_task_specification(+TaskSpec,-Task,-Inputs,-Options)
% process different forms of specifying patterns for running a 
% particular task within a model
% e.g. 
% task1([file1,file2]).
parse_task_specification(TaskSpecification,Task,Inputs,[]) :-
	TaskSpecification =.. [ Task, Inputs ],
	is_list(Inputs).
% or
% task1([file1,file2],[opt1(foo),opt2(bar)]).		
parse_task_specification(TaskSpecification,Task,Inputs,Options) :-
	TaskSpecification =.. [ Task, Inputs, Options ],
	is_list(Inputs),
	is_list(Options).
% or
% task1(file1,file2).
parse_task_specification(TaskSpecification,Task,Inputs,[]) :-
	TaskSpecification =.. [ Task | Inputs ],
	is_list(Inputs).
		
				
	
