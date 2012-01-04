:- module(script, [run/1, run_parallel/1, dryrun/1]).
/** <module> The LoSt scripting language

This is the library for running scripts in the LoSt script language.

The basic way of running a script is to consult the script from PRISM
and the call run/1 to invoke the goal of interest in the script.  

*/

% The idea is to formulate a declarative (non-procedural way of specifying models)
:- op(1200,xfx,'<-'). % same precendece as :-
:- op(1050,xfx,'::').
:- op(1100,xfx,'|').

:- lost_include_api(misc_utils).
:- lost_include_api(scheduler).
:- lost_include_api(scheduler_tree).

% Simple Prolog debugging trick
:- op(800, fx,'>').

'>'(X) :-
        writeln(call(X)),
        call(X),
		writeln(return(X)),nl.

list_lost_rules :- listing('<-'/2).

rerun(Target) :-
	run(Target,[rerun(once)]).

rerun_recursive(Target) :-
	run(Target,[rerun(recursive)]).
	
%% view(+Goal)
% Launch external viewer to view the file corresponding to script goal: Goal. Currently, just launches vim. 
% If the goal has not yet been run and the result is not yet available on file, then the goal will be run
% before launching the file viewer.
view(Target) :-
	run(Target,[],File),
	atom_concat('vim ', File,ViewCmd),
	system(ViewCmd).
	
%% get_result_file(+Goal,-File)
% File is unified to the result file produced by running Goal
get_result_file(Goal,File) :-
	run(Goal,[],File).
	
run_options([rerun(recursive)],[caching(false)],rerun(recursive)).
run_options([rerun(once)],[caching(false)],[]).
run_options([],[caching(true)],[]).


%% run(+Goal)
% Will run the script goal Goal with the sequential semantics (one task/process at a time).  
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
	
run(lost_data_file(Identifier),_RunOpts,File) :-
	atom(Identifier),
	lost_data_file(Identifier,File).
	
run(lost_sequence_file(Identifier),_RunOpts,File) :-
	atom(Identifier),
	lost_sequence_file(Identifier,File).

run(file(File),_RunOpts,File) :-
	atom(File),
	write('MAtch file!!'),nl,
	% Check that file exists
	(file_exists(File) ->
		true
		;
		atom_concat_list(['File ', File, ' does not exist!'],ErrMsg),
		throw(ErrMsg)).

% Run with procotol identifiers
run(Target,_RunOpts,File) :-
	atom(Target),
	atom_codes(Target,TargetSyms),
	atom_codes('file://', MatchSyms),
	append(MatchSyms,FileCodes,TargetSyms),
	atom_codes(File,FileCodes).
	
run(Target,_RunOpts,Target) :-
	atom(Target),
	atom_codes(Target,TargetSyms),
	map(atom_codes,['ftp://','http://', '"ftp://', '"http://'],Matchers),
	member(MatchSyms,Matchers),
	append(MatchSyms,_,TargetSyms).
	
run(Target,RunOpts,File) :-
	clause('<-'(Target,Rule),true),
	parse_guard_and_body(Rule,Guard,Model,TaskSpec),
	write('GUARD:'),writeln(Guard),
	call(Guard),
	parse_task_specification(TaskSpec,Task,Inputs,Options),
	run_options(RunOpts,RunModelOptions,NewRunOpts),
	findall(DependencyFile,(member(Dependency,Inputs), writeln(run(Dependency,NewRunOpts,DependencyFile)),run(Dependency,NewRunOpts,DependencyFile)), InputFiles),
	RealTaskSpec =.. [ Task, InputFiles, Options, File ],
	writeln(RealTaskSpec),
	run_model(Model,RealTaskSpec,RunModelOptions).

run(Target,_RunOpts,_File) :-
	write('failed to run target: '), 
	write(Target),nl,
%	writeln('Valid targets are: '),
%	findall(T1,clause('<-',T1,_),Targets),
%	forall(member(T,Targets),(write('\t* '),write(T),nl)),
	!,
	fail.	


parse_guard_and_body(Spec, true, Model, TaskSpec) :-
%	write(Spec),nl,
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parallel execution
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% run_parallel(+Goal)
% Runs the Goal using the parallel semantics. 
run_parallel(Target) :-
	generate_call_specs(Target,[],CallSpecs),
	writeln(CallSpecs),
	writeln('-------------'),
	build_scheduler_tree(CallSpecs,SchedulerTree),
	writeln('-------------'),
	scheduler_tree_print(SchedulerTree),!,
	writeln('reducing tree'),nl,
	scheduler_tree_reduce(SchedulerTree,ReducedTree),!,
	scheduler_tree_print(ReducedTree),
	scheduler_init,
	scheduler_loop(ReducedTree,[]),
	scheduler_shutdown.

simplify_goal(Goal,Simplified) :-
	Goal =.. [ Functor, InFiles, Opts, OutFile ],
	file_base_name(OutFile,SimplerOutFile),
	map(file_base_name,InFiles,SimplerInFiles),
	Simplified =.. [ Functor, SimplerInFiles, Opts, SimplerOutFile ].

build_scheduler_tree(CallSpec,FinalTree) :-
	scheduler_tree_create(EmptyTree),
	build_scheduler_tree_rec(nil,CallSpec,EmptyTree,FinalTree).

build_scheduler_tree_rec(_,[],Tree,Tree).
	
build_scheduler_tree_rec(_,[(nil,nil,_)],Tree,Tree).

build_scheduler_tree_rec(Parent,[(Model,Goal,_),ChildCalls],InTree,OutTree) :-
	scheduler_tree_add(Model,Goal,Parent,InTree,OutTree1,TaskId),
	build_scheduler_tree_rec(TaskId,ChildCalls,OutTree1,OutTree).

build_scheduler_tree_rec(Parent,[[(Model,Goal,_),Children]|Siblings],InTree,OutTree) :-
	scheduler_tree_add(Model,Goal,Parent,InTree,Tree1,TaskId),
	build_scheduler_tree_rec(TaskId,Children,Tree1,Tree2),
	build_scheduler_tree_rec(Parent,Siblings,Tree2,OutTree).
	
generate_call_specs(lost_data_file(Identifier),_RunOpts,(nil,nil,File)) :-
	atom(Identifier),
	lost_data_file(Identifier,File).

generate_call_specs(file(File),_RunOpts,(nil,nil,File)) :-
	atom(File),
	% Check that file exists
	(file_exists(File) ->
		true
		;
		atom_concat_list(['File ', File, ' does not exist!'],ErrMsg),
		throw(ErrMsg)).

% Run with procotol identifiers
generate_call_specs(Target,_RunOpts,(nil,nil,File)) :-
	atom(Target),
	atom_codes(Target,TargetSyms),
	atom_codes('file://', MatchSyms),
	append(MatchSyms,FileCodes,TargetSyms),
	atom_codes(File,FileCodes).

generate_call_specs(Target,_RunOpts,(nil,nil,Target)) :-
	atom(Target),
	atom_codes(Target,TargetSyms),
	map(atom_codes,['ftp://','http://', '"ftp://', '"http://'],Matchers),
	member(MatchSyms,Matchers),
	append(MatchSyms,_,TargetSyms).

generate_call_specs(Target,RunOpts,[(Model,RunGoal,OutputFile),ChildSpecs]) :-
	clause('<-'(Target,Rule),true),
	parse_guard_and_body(Rule,Guard,Model,TaskSpec),
	call(Guard), % Make sure that the guard holds
	parse_task_specification(TaskSpec,Task,Inputs,Options),
	run_options(RunOpts,_RunModelOptions,NewRunOpts),
	findall([(ChildModel,ChildGoal,ChildOutputFile),SubSpecs],
		(
			member(Dependency,Inputs),
			generate_call_specs(Dependency,NewRunOpts,[(ChildModel,ChildGoal,ChildOutputFile),SubSpecs])
		),
		ChildSpecs),
	map(callspec_output_file,ChildSpecs,InputFiles),
	RunGoal =.. [ Task, InputFiles, Options, OutputFile ],
	goal_result_file(Model,RunGoal,OutputFile).

callspec_output_file([(_Model,_Goal,OutputFile),_],OutputFile).
