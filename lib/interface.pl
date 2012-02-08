:- module(interface, []).
/** <module> interface
author: Christian Theil Have

Module that defines main predicates of the framework.
*/

% APIs used
:- use([path,misc_utils,io,prologdb,annotation_index,script,misc_utils,errorcheck,debug]).

%:- lost_include_api(help).

%% run_lost_model(+Model,+Goal)
%
% Can be called as,
% ==
% run_model(Model, Goal)
% ==
%
% Model: The name of the model to be run
%
% Goal must be of the from, Goal =.. [ F, InputFiles, Options, OutputFile ]
% InputFiles: A list of filenames given as input to the model.
% Options: A list of options to the model.

% Run a Lost Model
run_model(Model,Goal) :-
	run_model(Model,Goal,[caching(true)]).

run_model(Model,Goal,RunModelOptions) :-
	debug(interface(run_model), run_model(Model,Goal,RunModelOptions)),
	Goal =.. [ Functor, Inputs, Options, Filenames ],
	lost_model_interface_file(Model, ModelFile),
	check_valid_model_call(Model,Functor,Inputs,Options),
	expand_model_options(Model, Functor, Options, ExpandedOptions),
	debug(interface(run_model), ['expanded options: ', ExpandedOptions]),
	% Check if a result allready exists:
	lost_data_index_file(AnnotIndex),
	debug(interface(run_model), ['filename pattern: ', Filenames]),
	declared_output_formats(Model,Functor,OutputFormats),
	(is_list(OutputFormats) ->
		length(OutputFormats,NumberOutputs),
		OutputFilesAsList = true
		;
		NumberOutputs = 1,
		OutputFilesAsList = false
	),
	length(Filenames,NumberOutputs),
	lost_file_index_get_filenames(AnnotIndex,Model,Functor,Inputs,ExpandedOptions,Filenames),
	debug(interface(run_model),['generated file names:', Filenames]),
	!,
	((member(caching(true),RunModelOptions),forall(member(Filename,Filenames),file_exists(Filename))) ->
		debug(interface(run_model), ['Using existing result files: ', Filenames])
		;
		(OutputFilesAsList ->
			CallGoal =.. [Functor,Inputs,ExpandedOptions,Filenames]
			;
			[SingleFilename] = Filenames,
			CallGoal =.. [Functor,Inputs,ExpandedOptions,SingleFilename]
		),
		term2atom(CallGoal,GoalAtom),
		debug(interface(run_model),[launch_prism_process(ModelFile,GoalAtom)]),
		launch_prism_process(ModelFile,GoalAtom),
		forall(member(Filename,Filenames), 
		check_or_fail(file_exists(Filename),interface_error(missing_annotation_file(Filename)))),
		lost_file_index_update_file_timestamp(AnnotIndex,Filenames)
	).

%% goal_result_files(+Model,+Goal,-ResultFiles)
% Map from a Model and Goal to the set of output files.
goal_result_files(Model,Goal,ResultFiles) :-
	lost_data_index_file(AnnotIndex),
	Goal =.. [ Functor, Inputs, Options, ResultFiles ],
	expand_model_options(Model, Functor, Options, ExpandedOptions),
	sort(ExpandedOptions,ExpandedSortedOptions),
	lost_file_index_get_filename(AnnotIndex,Model,Functor,Inputs,ExpandedSortedOptions,ResultFiles).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Check declared options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% verify_models_options_declared(+Model,+Goal,+Options)
%
% Fail if a given option is undeclared.
verify_model_options_declared(Model,Goal,Options) :-
	declared_model_options(Model,Goal,DeclaredOptions),
	writeln(DeclaredOptions),
	option_is_declared(Options,DeclaredOptions).

option_is_declared([], _).
option_is_declared([Option|Rest], DeclaredOptions) :-
	Option =.. [ OptionName, _ ],
	OptionMatcher =.. [ OptionName, _ ],
	member(OptionMatcher,DeclaredOptions),
	option_is_declared(Rest,DeclaredOptions).

%% expand_model_options(+Model,+Goal,+Options,-ExpandedOptions)
%
% Expand the set of given options to contain options with default
% values as declared by the model
expand_model_options(Model, Goal, Options, SortedOptions) :-
	declared_model_options(Model, Goal, DeclaredOptions),
	expand_options(DeclaredOptions,Options,DefaultOptions),
	append(Options,DefaultOptions,ExpandedOptions),
	sort(ExpandedOptions,SortedOptions).

expand_options([],_,[]).

% The declared option is part of given options
expand_options([DeclaredOption|Ds],Options,Rest) :-
	DeclaredOption =.. [ Key, _Value ],
	OptionMatcher =.. [ Key, _ ],
	member(OptionMatcher, Options),
	expand_options(Ds,Options,Rest).

% The declared option is not part of given options:
% Add it with a default value
expand_options([DeclaredOption|Ds],Options,[DefaultOption|Rest]) :-
	DeclaredOption =.. [ Key, Default ],
	OptionMatcher =.. [Key,_],
	not(member(OptionMatcher, Options)),
	DefaultOption =.. [Key,Default],
	expand_options(Ds,Options,Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Option parsing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TODO: Eventually we should have some

%% get_option(+Options,-OptionNameValue)
%% get_option(+Options,+OptionName,-Value)
%
% Predicate to get the value of an option OptionName
get_option(Options, KeyValue) :-
	member(KeyValue, Options).

get_option(Options, Key, Value) :-
        KeyValue =.. [ Key, Value ],
	get_option(Options,KeyValue).

lost_required_option(Options, Key, Value) :-
	write('!!! lost_required_option is deprecated since ALL declared options are now required. Use get_option/3 instead !!!'),nl,
	get_option(Options,Key,Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Launching a PRISM process
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% launch_prism_process(+PrismPrologFile,+Goal)
%
% Launch a prism process that first consults
% the file named by PrismPrologFile and then
% execute the goal Goal.
launch_prism_process(PrismPrologFile, Goal) :-
	write('####################################################################'),nl,
	write('# Launching new PRISM process                                      #'),nl,
	write('####################################################################'),nl,
	working_directory(CurrentDir),
	file_directory_name(PrismPrologFile,Dirname),
	file_base_name(PrismPrologFile,Filename),
	chdir(Dirname),
	% Build PRISM command line:
	lost_config(prism_command,PRISM),
	lost_config(lost_base_directory,LostBaseDir),
	atom_concat_list([PRISM,' -g "', 'consult(\'',LostBaseDir,'/lost.pl','\'), use(interface), ','consult(\'',Filename,'\'), ',Goal,'"'],Cmd),
	debug(interface(launch_prism_process),['working directory: ', Dirname ]),
	debug(interface(launch_prism_process),['cmd: ', Cmd]),
	% FIXME: Setup some stdout redirection (this may be troublesome on windows)
	% Run PRISM
	% The following UGLY code is to catch arbitrary memory faults (ExitCode 11) - retry the command at most ten times **OTL**
	% should be changed to wrap the one main system command	 **OTL**
	system(Cmd,ExitCode),
	(
	ExitCode == 11 ->
		write('--> PRISM process exits with code 11 on first attempt, retrying ...'),nl,
		retry_sys_command(Cmd,9,ExtraAttempts,ExitCode2)
	;
		ExitCode2 = ExitCode,
		ExtraAttempts = 0
	),
	Attempts is 1 + ExtraAttempts,
	write('--> PRISM process exits with code '), write(ExitCode2), write(', total attempts '), write(Attempts),nl,
	% Unfortunately bprolog/prism doesn't support standard exit codes:
	check_or_fail((ExitCode2==0), error(launch_prism_process(PrismPrologFile,Goal))),
    chdir(CurrentDir).


%% retry_sys_command(+System Command,+MaxAttempts, -Attempts, -ExitCode)
%
% Retry a system command until exitcode differetn from 11 or MaxAttempts times:
% return total number of attempts and final exitcode
retry_sys_command(SysCommand, MaxTries, FinalTry, Code):-
	retry_rec(SysCommand, MaxTries, 0 , FinalTry,Code).

retry_rec(_SysCommand, MaxTries, MaxTries, MaxTries ,11).
retry_rec( SysCommand, MaxTries, Counter,  FinalTry, Code):-
	MaxTries > Counter,
	ThisTry is Counter +1,
	system(SysCommand,ExitCode),
	(
	ExitCode == 11 ->
		retry_rec(SysCommand, MaxTries, ThisTry, FinalTry, Code)
	;
		FinalTry = ThisTry,
		Code = ExitCode
	).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lost model interface analysis
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% check_valid_model_call(+Model,+Task,+Options)
% Check that a task Task is declared in Model and that the task is called with correct options, and input file arguments
check_valid_model_call(Model,Task,_InputFiles,Options) :-
	lost_model_interface_file(Model, ModelFile), 
	% Verify that that interface file exists
	check_or_fail(file_exists(ModelFile),interface_error(missing_interface_file(ModelFile))), 
	% Check that the task is declared
	check_or_fail(task_declaration(Model,Task,_TaskDecl), error(no_task_declaration(Model,Task))),
	% Checks there is an implementation of declared task
	check_or_fail(task_has_implementation(Model,Task), error(no_task_implementation(Model,Task))),
	% Check that the task is not call with unknown model options
	debug(interface(check_valid_model_call),[' options: ',Options]),
	check_or_fail(verify_model_options_declared(Model,Task,Options), error(interface(model_called_with_undeclared_options(Model,Options)))).
	/*
	check_or_warn(lost_interface_input_formats(Model,Task, _), warning(interface(missing_input_formats_declaration(Model,Task)))),
	check_or_warn(lost_interface_defines_output_format(Model,Task), warning(interface(missing_output_format_declaration(Model,Task)))).
	*/

%% task_has_implementation(+Model,+Task)
% True if the there is a predicate implementing task in Model.
% Note that this merely verifies that a predicate of the correct name and arity exists, but not whether it works.
task_has_implementation(Model,Task) :-
	lost_model_interface_file(Model,ModelFile),
	terms_from_file(ModelFile, Terms),
	terms_has_rule_with_head(Terms,Task,3).
	
%% task_declaration(+Model,+Task,-Declaration)
% True if Model as a declared task named Task
task_declaration(Model,Task,Declaration) :-
	lost_model_interface_file(Model, ModelFile),
	terms_from_file(ModelFile, Terms), 
	TaskMatcher =.. [ ':-', task(Declaration) ], 
	member(TaskMatcher,Terms),
	task_name(Declaration,Task).

%% declared_model_options(+Model,+Task,-DeclaredOptions)
% DeclaredOptions is a list of declared for the task identified by functor Goal in model Model.
declared_model_options(Model, Task, DeclaredOptions) :-
	task_declaration(Model,Task,TaskDeclaration),
	task_options(TaskDeclaration,DeclaredOptions).

%% declared_input_formats(+Model,+Task,-Formats)
% Formats is the list of input formats for the task Task in Model.
declared_input_formats(Model,Task,Formats) :-
	task_declaration(Model,Task,TaskDeclaration),
	task_name(TaskDeclaration,Task),
	task_input_filetypes(TaskDeclaration,Formats).

%% declared_output_formats(+Model,+Task,-Formats)
% Formats is the formats of the output file of task Task in Model
declared_output_formats(Model,Task,OutputFormats) :-
	task_declaration(Model,Task,TaskDeclaration),
	task_name(TaskDeclaration,Task),
	task_output_filetypes(TaskDeclaration,OutputFormats).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Time stamp checking
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

file_modification_time(File,Timestamp) :-
	lost_data_index_file(IndexFile),
	lost_file_index_filename_member(IndexFile,File),
	lost_file_index_get_file_timestamp(IndexFile,File,Timestamp).

% To be tested...
file_modification_time(File,T) :-
        lost_config(platform,windows),
        file_property(File,modification_time(T)).

% Stupid hack to work around bug that makes bprolog/prism segfault
file_modification_time(File,time(Year,Mon,Day,Hour,Min,Sec)) :-
	lost_config(platform,unix),
        lost_tmp_directory(TmpDir),
        atom_concat(TmpDir,'filestat.tmp',TmpFile),
	atom_concat_list(['stat ', File,
                          '|grep Modify ',
			  '|cut -d. -f1 ',
			  '|cut -d" " -f2,3 ',
			  '> ', TmpFile],Cmd),
	system(Cmd),
	readFile(TmpFile,Contents),
	Contents = [
                Y1,Y2,Y3,Y4,45,Mon1,Mon2,45,Day1,Day2,
                32,
                H1,H2,58,M1,M2,58,S1,S2|_],
	atom_codes(YearA,[Y1,Y2,Y3,Y4]),
        parse_atom(YearA,Year),
	atom_codes(MonA,[Mon1,Mon2]),  parse_atom(MonA,Mon),
	atom_codes(DayA,[Day1,Day2]),  parse_atom(DayA,Day),
	atom_codes(HourA,[H1,H2]), parse_atom(HourA,Hour),
	atom_codes(MinA,[M1,M2]),  parse_atom(MinA,Min),
        atom_codes(SecA,[S1,S2]),  parse_atom(SecA,Sec).

% True if all Files and dependency files of those file are no
% older than the time given as second argument.
% E.g. if the file is a generated annotation file, then we must check whether
% this file has depends on other files which may be outdated
rec_files_older_than_timestamp([],_).

rec_files_older_than_timestamp([File|FilesRest],TS1) :-
	timestamp_to_list(TS1,TS1List),
	lost_data_index_file(IndexFile),
	lost_file_index_filename_member(IndexFile,File),
	file_modification_time(File,TS2),
	timestamp_to_list(TS2,TS2List),
	timestamp_list_after(TS1List,TS2List),
	lost_file_index_inputfiles(IndexFile,File,DependencyFiles),
	rec_files_older_than_timestamp(FilesRest,TS1),
	rec_files_older_than_timestamp(DependencyFiles,TS1).

rec_files_older_than_timestamp([File|FilesRest],TS1) :-
	timestamp_to_list(TS1,TS1List),
	file_modification_time(File,TS2),
	timestamp_to_list(TS2,TS2List),
	timestamp_list_after(TS1List,TS2List),	
	rec_files_older_than_timestamp(FilesRest,TS1).

% True if timestamp list in first argument is after (or same)
% as timestamp list in second argument
timestamp_list_after([],[]).
timestamp_list_after([Elem1|_],[Elem2|_]) :-
	Elem1 > Elem2.
timestamp_list_after([Elem1|Rest1],[Elem2|Rest2]) :-
	Elem1 == Elem2,	
	timestamp_list_after(Rest1,Rest2).

timestamp_to_list(time(Year,Mon,Day,Hour,Min,Sec), [Year,Mon,Day,Hour,Min,Sec]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tasks 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% task_name
% true if TaskDeclaration declares a task with functor Name
task_name(TaskDeclaration,Name) :-
	TaskDeclaration =.. [ Name | _ ].
	
%% task_input_filetypes(+TaskDeclaration,-InputFileTypes)
% InputFileTypes is a list of the input file types for the task identified by TaskDeclaration
task_input_filetypes(TaskDeclaration,InputFileTypes) :-
	TaskDeclaration =.. [ _Name , InputFileTypes, _, _ ],
	(InputFileTypes = [_|_] ; InputFileTypes = []),
	!.

task_input_filetypes(TaskDeclaration,[InputFileType]) :-
	TaskDeclaration =.. [ _Name , InputFileType, _, _ ].

%% task_options(+TaskDeclaration,-Options)
% Options is the list of options declarared by TaskDeclaration
task_options(TaskDeclaration,Options) :-
	TaskDeclaration =.. [ _Name , _InputFileTypes, Options, _ ].
	
%% task_output_filetypes(+TaskDeclaration,-OutputFileType)
% OutputFileType is the file type declared by TaskDeclaration
task_output_filetypes(TaskDeclaration,OutputFileTypes) :-
	TaskDeclaration =.. [ _Name , _InputFileTypes, _Options, OutputFileTypes ].
/*
	OutputFileTypes = [_|_],
	!.
	
task_output_filetypes(TaskDeclaration,[OutputFileType]) :-
	TaskDeclaration =.. [ _Name , _InputFileTypes, _Options, OutputFileType ].
*/	
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Some utilitites
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% list_lost_models_to_file(File)
%
% Write in File the list of lost models
% This is used by the CLC gui
list_lost_models_to_file(File) :-
	open(File,write,OStream),
	list_lost_models(Models),
	write(OStream,lost_models(Models)),
	write(OStream,'.\n'),
	close(OStream).

%% list_lost_models(-Models)
%
% Compute the list of models available
list_lost_models(Models) :-
	lost_models_directory(ModelsDir),
    getcwd(C),
    cd(ModelsDir),
	directory_files('.',Files),
    cd(C),
	subtract(Files,['.','..'],Models).

directories_in_list([],[]).

directories_in_list([File|FileList],[File|DirectoryList]) :- 
	file_property(File,directory),
	!,
	directories_files(FileList,DirectoryList).

directories_in_list([_|FileList],DirectoryList) :-
	directories_in_list(FileList,DirectoryList).

list_lost_model_options_to_file(Model,Goal,OutputFile) :-
	open(OutputFile,write,OStream),
	declared_model_options(Model, Goal, Options),
	write_model_options(OStream, Options),
	close(OStream).
% where
write_model_options(_,[]).
write_model_options(OStream, [Option1|Rest]) :-
	writeq(OStream, Option1),
	write(OStream, '.\n'),
	write_model_options(OStream,Rest).

%% lost_model_input_formats_to_file(+Model,+Goal,+OutputFile)
%
% Write the input formats of a Model called with Goal to the file OutputFile
% Used by the CLC gui 
lost_model_input_formats_to_file(Model,Goal,OutputFile) :-
	lost_interface_input_formats(Model,Goal,Formats),
	open(OutputFile,write,OStream),
	write(OStream,lost_input_formats(Model,Goal,Formats)),
	write(OStream, '.\n'),
	close(OStream).

%% lost_mode.output_format_to_file
% 
% Write the output formats of a Model called with Goal/Options to OutputFile
% API call required by the CLC gui
lost_model_output_format_to_file(Model,Goal,Options,OutputFile) :-
	lost_interface_output_format(Model,Goal,Options,OutputFormat),
	open(OutputFile,write,OStream),
	write(OStream,lost_model_output_format(Model,Goal,Options,OutputFormat)),
	write(OStream, '.\n'),
	close(OStream).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% System 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear :- system(clear).

