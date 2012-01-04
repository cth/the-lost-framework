% The idea is to formulate a declarative (non-procedural way of specifying models)
:- op(1200,xfx,'<-'). % same precendece as :-
:- op(1050,xfx,'::').
:- op(1100,xfx,'|').


list_lost_rules :-
	listing('<-'/2).

		
rerun(Target) :-
	run(Target,[rerun(once)]).

rerun_recursive(Target) :-
	run(Target,[rerun(recursive)]).
	
view(Target) :-
	run(Target,[],File),
	atom_concat('vim ', File,ViewCmd),
	system(ViewCmd).
	
run_options([rerun(recursive)],[caching(false)],rerun(recursive)).
run_options([rerun(once)],[caching(false)],[]).
run_options([],[caching(true)],[]).

run(Target) :-
	run(Target,[]).
	
run(Target,Opts) :-
 	write('run: '), write(Target),nl,
	run(Target,Opts,File),
	write(run(Target,Opts,File)),nl,
	write('Success '),
	write(Target),
	write(' ==> '),
	write(File),
	nl.
	


/*generate_call_specs(Target,_RunOpts,_File) :-
	write('failed to run target: '), 
	write(Target),nl,
	!,
	fail.
*/
	


parse_guard_and_body(Spec, true, Model, TaskSpec) :-
	Spec =.. [ '::', Model, TaskSpec].
	
parse_guard_and_body(Spec, Guard, Model, TaskSpec) :-
	Spec =.. [ '|', Guard, Body ],
	Body =.. [ '::', Model, TaskSpec].
	
%%
% parse_task_specification(+TaskSpec,-Task,-Inputs,-Options)
% process different forms of specifying patterns for running a 
% particular task within a model
% e.g. 
%   task1([file1,file2]).
parse_task_specification(TaskSpecification,Task,Inputs,[]) :-
	TaskSpecification =.. [ Task, Inputs ],
	is_list(Inputs).
% or
%   task1([file1,file2],[opt1(foo),opt2(bar)]).		
parse_task_specification(TaskSpecification,Task,Inputs,Options) :-
	TaskSpecification =.. [ Task, Inputs, Options ],
	is_list(Inputs),
	is_list(Options).
	
% or 
% 	task1(file,[opt1(foo)])
parse_task_specification(TaskSpecification,Task,[Inputs],Options) :-
	TaskSpecification =.. [ Task, Inputs, Options ],
	not(is_list(Inputs)),
	is_list(Options),
	!.

% or
%    task1(file1,file2).
parse_task_specification(TaskSpecification,Task,Inputs,[]) :-
	TaskSpecification =.. [ Task | Inputs ],
	is_list(Inputs),
	write(TaskSpecification),nl,
	forall(member(L,Inputs),not(is_list(L))).
	
