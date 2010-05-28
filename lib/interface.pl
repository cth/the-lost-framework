:- lost_include_api(misc_utils).
:- lost_include_api(io).

% Simple Prolog debugging trick
:- op(800, fx,'>').
'>'(X) :- writeq(X), write('\n'), call(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get_annotation_file/4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Will unify Filename with the name of the containing an annotation
% the the prism run with Model, and Inputs. If no such annotation
% exists, PRISM will be run to generate it.
% Model: The name of the model to be run
% Inputs: A list of filenames given as input to the model.
% Options: A list of options to the model.
get_annotation_file(Model, Inputs, Options, Filename) :-
	lost_model_interface_file(Model, ModelFile),	
	check_valid_model_call(Model, lost_best_annotation,3, Options),
	expand_model_options(Model, Goal, Options, ExpandedOptions),
	sort(ExpandedOptions,ExpandedSortedOptions),
	% Check if a result allready exists:
	lost_annotation_index_file(AnnotIndex),
	lost_file_index_get_filename(AnnotIndex,Model,Inputs,ExpandedSortedOptions,Filename),
	lost_file_index_get_file_timestamp(AnnotIndex,Filename,Timestamp),
	((file_exists(Filename),rec_files_older_than_timestamp(Inputs,Timestamp)) ->
	 write('Using existing annotation file: '), write(Filename),nl
	;
	 term2atom(lost_best_annotation(Inputs,ExpandedSortedOptions,Filename),Goal),
         write(launch_prism_process(ModelFile,Goal)),
	 launch_prism_process(ModelFile,Goal),
	 check_or_fail(file_exists(Filename),interface_error(missing_annotation_file(Filename))),
	 lost_file_index_update_file_timestamp(AnnotIndex,Filename)
	).

% Alias for get_annotation_file...
run_model(Model,Inputs,Options,Filename) :-
	get_annotation_file(Model,Inputs,Options,Filename).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% train_model/4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

train_model(Model, TrainingDataFiles, Options, SavedParamsFile) :-
	lost_model_interface_file(Model, ModelFile),	
	check_valid_model_call(Model, lost_learn,3, Options),
	expand_model_options(Model, Goal, Options, ExpandedOptions),
	sort(ExpandedOptions,ExpandedSortedOptions),	
	lost_model_parameter_index_file(Model,ParamFileIndex),
	lost_file_index_get_filename(ParamFileIndex,Model,TrainingDataFiles,ExpandedSortedOptions,SavedParamsFile),
	write(lost_file_index_get_filename(ParamFileIndex,Model,TrainingDataFiles,ExpandedSortedOptions,SavedParamsFile)),nl,
	!,
	(file_exists(SavedParamsFile) ->
	 write('Using existing parameter file: '), write(SavedParamsFile), nl
	 ;
	 term2atom(lost_learn(TrainingDataFiles,ExpandedSortedOptions,SavedParamsFile),Goal),
	 launch_prism_process(ModelFile,Goal),
	 check_or_fail(file_exists(SavedParamsFile),interface_error(missing_parameter_file(SavedParamsFile))),
	 lost_file_index_update_file_timestamp(ParamFileIndex,SavedParamsFile)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Check declared options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Fail if a given option is undeclared.
verify_model_options_declared(Model,Goal,Options) :-
	declared_model_options(Model,Goal,DeclaredOptions),
	option_is_declared(Options,DeclaredOptions).

option_is_declared([], _).
option_is_declared([Option|Rest], DeclaredOptions) :-
	Option =.. [ OptionName, _ ],
	DeclaredOptionMatcher =.. [ lost_option, _, OptionName, _, _],
	member(DeclaredOptionMatcher,DeclaredOptions),
	option_is_declared(Rest,DeclaredOptions).

% Expand the set of given options to contain options with default
% values as declared by the model
expand_model_options(Model, Goal, Options, ExpandedOptions) :-
	declared_model_options(Model, Goal, DeclaredOptions),
	expand_options(DeclaredOptions,Options,DefaultOptions),
	append(Options,DefaultOptions,ExpandedOptions).

expand_options([],_,[]).

% The declared option is part of given options
expand_options([lost_option(_,OptionName,_,_)|Ds],Options,Rest) :-
	OptionMatcher =.. [OptionName,_],
	member(OptionMatcher, Options),
	expand_options(Ds,Options,Rest).

% The declared option is not part of given options:
% Add it with a default value
expand_options([lost_option(_,OptionName,DefaultValue,_)|Ds],Options,[DefaultOption|Rest]) :-
	OptionMatcher =.. [OptionName,_],
	not(member(OptionMatcher, Options)),
	DefaultOption =.. [OptionName,DefaultValue],
	expand_options(Ds,Options,Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Option parsing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TODO: Eventually we should have some

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
	atom_concat_list([PRISM,' -g "cl(\'',Filename,'\'), ',Goal,'"'],Cmd),
	write('working directory: '), write(Dirname), nl,
	write('cmd: '), write(Cmd),nl,
	% FIXME: Setup some stdout redirection (this may be troublesome on windows)
	% Run PRISM
	system(Cmd,ExitCode),
	write('--> PRISM process exits with code '), write(ExitCode),nl,
	% Unfortunately bprolog/prism does get the concept of exit codes. DOH!!!
	check_or_fail((ExitCode==0), error(launch_prism_process(PrismPrologFile,Goal))),
	chdir(CurrentDir).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Directory and file management
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lost_tmp_directory(TmpDir) :-
        lost_config(lost_base_directory,BaseDir),!,
        atom_concat(BaseDir,'/tmp/', TmpDir).

lost_models_directory(ModelsDir) :-
	lost_config(lost_base_directory, Basedir),!,
	atom_concat(Basedir, '/models/', ModelsDir).

lost_data_directory(Dir) :-
	lost_config(lost_base_directory, Basedir),!,
	atom_concat(Basedir,'/data/',Dir).

lost_model_directory(Model,ModelDir) :-
	lost_models_directory(ModelsDir),
	atom_concat(ModelsDir,Model,ModelDir1),
	atom_concat(ModelDir1,'/',ModelDir).

lost_model_parameters_directory(Model,Dir) :-
	lost_model_directory(Model, ModelDir),
	atom_concat(ModelDir, 'parameters/',Dir).

lost_model_annotations_directory(Model,Dir) :-
	lost_model_directory(Model, ModelDir),
	atom_concat(ModelDir, 'annotations/',Dir).

lost_model_interface_file(Model,ModelFile) :-
	lost_model_directory(Model,ModelDir),
	atom_concat(ModelDir, 'interface.pl',ModelFile).

lost_model_parameter_index_file(Model,IndexFile) :-
	lost_model_parameters_directory(Model,Dir),
	atom_concat(Dir,'parameters.idx',IndexFile).

% FIXME: This one is likely to change
lost_model_parameter_file(Model,ParameterId,ParameterFile) :-
	lost_model_parameters_directory(Model,Dir),
	atom_concat(Dir,ParameterId,ParameterFile1),
	atom_concat(ParameterFile1,'.prb',ParameterFile).

lost_annotation_index_file(IndexFile) :-
	lost_data_directory(AnnotDir),
	atom_concat(AnnotDir,'annotations.idx',IndexFile).

lost_data_file(SequenceId, SequenceFile) :-
	lost_data_directory(D),
	atom_concat(SequenceId,'.seq', Filename),
	atom_concat(D, Filename, SequenceFile).

lost_sequence_file(SequenceId, SequenceFile) :-
	lost_data_file(SequenceId, SequenceFile).


is_generated_file(File_Path) :-
        lost_data_directory(D),
        atom_concat(D,Name_File,File_Path),
        atom_concat(_,'.gen',Name_File).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lost annotation index.
% rule prefix: lost_file_index_
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The index file contains a list of facts on the form
% fileid(FileId,Filename,Model,InputFiles,Options)
% where InputFiles and Options are a lists.

% Retrieve the filename matching (Model,Options,InputFiles) from the file index
% If no such filename exists in the index, then a new unique filename is created
% and unified to Filename 
lost_file_index_get_filename(IndexFile,Model,InputFiles,Options,Filename) :-
	(file_exists(IndexFile) -> terms_from_file(IndexFile,Terms) ; Terms = []),
	(lost_file_index_get_filename_from_terms(Terms,Model,InputFiles,Options,Filename) ->
	 true
	;
	 % create and reserver new filename:
	 lost_file_index_next_available_index(Terms, Index),
	 dirname(IndexFile,IndexDir),
	 term2atom(Index,IndexAtom),
	 atom_concat_list([IndexDir, Model, '_',IndexAtom,'.gen'], Filename),
	 lost_file_index_timestamp(Ts),
	 term2atom(Ts,AtomTs),
	 append(Terms, [fileid(IndexAtom,AtomTs,Filename,Model,InputFiles,Options)],NewTerms),
	 terms_to_file(IndexFile,NewTerms)
	).

% Given Terms, unify NextAvailableIndex with a unique index not
% occuring as index in  terms:
lost_file_index_next_available_index(Terms, NextAvailableIndex) :-
	lost_file_index_largest_index(Terms,LargestIndex),
	NextAvailableIndex is LargestIndex + 1.

% Unify second argument with largest index occuring in terms
lost_file_index_largest_index([], 0).
lost_file_index_largest_index([Term|Rest], LargestIndex) :-
	Term =.. [ fileid, TermIndexAtom | _ ],
	atom2integer(TermIndexAtom,Index),
	lost_file_index_largest_index(Rest,MaxRestIndex),
	max(Index,MaxRestIndex,LargestIndex).

% lost_file_index_get_filename_from_terms/5:
% Go through a list terms and check find a Filename matching (Model,ParamsId,InputFiles)
% Fail if no such term exist
lost_file_index_get_filename_from_terms([Term|_],Model,InputFiles,Options,Filename) :-
	Term =.. [ fileid, _, _, Filename, Model, InputFiles, Options ],
	!.

lost_file_index_get_filename_from_terms([_|Rest],Model,InputFiles,Options,Filename) :-
	lost_file_index_get_filename_from_terms(Rest,Model,InputFiles,Options,Filename).

% Get a timestamp corresponding to the current time
lost_file_index_timestamp(time(Year,Mon,Day,Hour,Min,Sec)) :-
	date(Year,Mon,Day),
	time(Hour,Min,Sec).

lost_file_index_get_file_timestamp(IndexFile,Filename,Timestamp) :-
	terms_from_file(IndexFile,Terms),
	TermMatcher =.. [ fileid,  _Index, TimeStampAtom, Filename, _Model, _InputFiles, _Options ],
	member(TermMatcher,Terms),
	parse_atom(TimeStampAtom,Timestamp).
				   
% Update the timestamp associated with Filename to a current timestamp
% This should be used if the file is (re) generated
lost_file_index_update_file_timestamp(IndexFile,Filename) :-
	lost_file_index_timestamp(Ts),
	term2atom(Ts,TsAtom),
	terms_from_file(IndexFile,Terms),
	OldTermMatcher =.. [ fileid,  Index, _, Filename, Model, InputFiles, Options ],
	subtract(Terms,[OldTermMatcher],TermsMinusOld),
	NewTerm =.. [ fileid,  Index, TsAtom, Filename, Model, InputFiles, Options ],
	append(TermsMinusOld,[NewTerm],UpdatedTerms),
	terms_to_file(IndexFile,UpdatedTerms).

lost_file_index_filename_member(IndexFile, Filename) :-
	terms_from_file(IndexFile,Terms),
	TermMatcher =.. [ fileid,  _Index, _Ts, Filename, _Model, _InputFiles, _Options ],
	member(TermMatcher,Terms).

lost_file_index_inputfiles(IndexFile,Filename,InputFiles) :-
	terms_from_file(IndexFile,Terms),
	TermMatcher =.. [ fileid,  _Index, _Ts, Filename, _Model, InputFiles, _Options ],
	member(TermMatcher,Terms).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lost model interface analysis
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_valid_model_call(Model, InterfacePredicate, Arity,Options) :-
	lost_model_interface_file(Model, ModelFile),
	check_or_fail(file_exists(ModelFile),interface_error(missing_model_file(ModelFile))),
	check_or_fail(lost_interface_supports(Model,InterfacePredicate,Arity),
		      interface_error(no_support(Model,InterfacePredicate/Arity))),
	check_or_warn(verify_model_options_declared(Model,InterfacePredicate,Options),
		      error(interface(model_called_with_undeclared_options(Model,Options)))),
	%write(lost_interface_input_formats(Model,InterfacePredicate, _)),nl,
	check_or_warn(lost_interface_input_formats(Model,InterfacePredicate, _),
		      warning(interface(missing_input_formats_declaration(Model,InterfacePredicate)))),
	check_or_warn(lost_interface_defines_output_format(Model,InterfacePredicate),
		      warning(interface(missing_output_format_declaration(Model,InterfacePredicate)))).

lost_interface_supports(Model,Functor,Arity) :-
	lost_model_interface_file(Model,ModelFile),
	terms_from_file(ModelFile, Terms),
	terms_has_rule_with_head(Terms,Functor,Arity).

declared_model_options(Model, Goal, DeclaredOptions) :-
	lost_model_interface_file(Model, ModelFile),
	terms_from_file(ModelFile, Terms),
	findall(Term, (member(Term,Terms), Term =.. [lost_option,Goal,_OptionName,_DefaultValue,_Description]),DeclaredOptions).

lost_interface_input_formats(Model,InterfacePredicate, Formats) :-
	lost_model_interface_file(Model,ModelFile),!,
	terms_from_file(ModelFile,Terms),!,
	InputDeclarationMatch =.. [ lost_input_formats, InterfacePredicate, Formats],
	member(InputDeclarationMatch, Terms).

lost_interface_defines_output_format(Model,InterfacePredicate) :-
	lost_model_interface_file(Model,ModelFile),
	terms_from_file(ModelFile, Terms),
	Head =.. [ lost_output_format, InterfacePredicate, _options, _format],
	Rule =.. [ :-, Head, _ ],
	write(Head),nl,
	write(Rule),nl,
	(member(Head, Terms) ; member(Rule,Terms)).

lost_interface_output_format_to_file(Model,InterfacePredicate, Options, OutputFormatFile) :-
	lost_model_interface_file(Model, ModelFile),
	lost_interface_defines_output_format(Model,InterfacePredicate),
	expand_model_options(Model,InterfacePredicate,Options,ExpandedOptions),
	sort(ExpandedOptions,ExpandedSortedOptions),
	% Delete OutputFormatFile if it allready exists
	% (file_exists(OutputFormatFile) -> delete_file(OutputFile) ; true),
	term2atom(call((lost_output_format(InterfacePredicate,ExpandedSortedOptions,OutputFormat),
			tell(OutputFormatFile),
			write(output_format(Model,InterfacePredicate,Options,OutputFormat)),
			atom_codes(Dot,[46]),
			write(Dot),
			nl,
			told)),
		  Goal),
	launch_prism_process(ModelFile,Goal).

% Determines the output format for a given Model and Interface given a list of options.
lost_interface_output_format(Model,InterfacePredicate,Options,OutputFormat) :-
	lost_tmp_directory(Tmp),
	atom_concat(Tmp, 'lost_interface_output_format.pl', Filename),
	lost_interface_output_format_to_file(Model,InterfacePredicate,Options,Filename),
	terms_from_file(Filename, [output_format(Model,InterfacePredicate,Options,OutputFormat)]).


lost_model_option_values_to_file(Model,InterfacePredicate,OptionName, OutputFile) :-
	lost_model_interface_file(Model, ModelFile),
	% Check if option is declared!!!
	% Delete OutputFile if it allready exists
	% (file_exists(OutputFile) -> delete_file(OutputFile) ; true),
	term2atom(call((lost_option_values(InterfacePredicate, OptionName, Values),
		       tell(OutputFile),
		       writeq(lost_option_values(Model,InterfacePredicate, OptionName, Values)),
		       atom_codes(Dot,[46]),
		       write(Dot),
		       nl,
		       told)),
		  Goal),
	launch_prism_process(ModelFile,Goal).

lost_model_option_values(Model, InterfacePredicate, OptionName, Values) :-
	lost_tmp_directory(Tmp),
	atom_concat(Tmp, 'lost_model_option_values.pl', Filename),
	lost_model_option_values_to_file(Model,InterfacePredicate,OptionName,Filename),
	terms_from_file(Filename, [lost_option_values(Model,InterfacePredicate,OptionName,Values)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Time stamp checking
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

file_modification_time(File,Timestamp) :-
	lost_annotation_index_file(IndexFile),
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
	lost_annotation_index_file(IndexFile),
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
% Some utilitites
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rm_gen :-
	lost_data_directory(D),
	atom_concat(D,'*.gen',FileGlobPattern),
	atom_concat('rm -f ', FileGlobPattern, Command),
	write(Command),nl,
	system(Command).

rm_tmp :-
	lost_tmp_directory(D),
	atom_concat(D,'*',FileGlobPattern),
	atom_concat('rm -f ', FileGlobPattern, Command),
	write(Command),nl,
	system(Command).


% Allows to query for models that support a particular 
% pattern of InputFiles, Options, and OutputFormat
%list_models_of_type(InputFiles,Options,OutputFormat,Models) :-
%	list_lost_models(AllModels),
%	findall(Model, (member(Model,AllModels), lost_model_is_of_type(Model,InputFiles,Options,OutputFormat)), Models).

%lost_model_is_of_type(Model,InputFiles,Options,OutputFormat) :-

list_lost_models_to_file(File) :-
	open(File,write,OStream),
	list_lost_models(Models),
	write(OStream,lost_models(Models)),
	write(OStream,'.\n'),
	close(OStream).
	
list_lost_models(Models) :-
	lost_models_directory(ModelsDir),
	directory_files(ModelsDir,Files),
	subtract(Files,['.','..'],Models).
%	directories_in_list(Files,Models).	

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

write_model_options(_,[]).
write_model_options(OStream, [Option1|Rest]) :-
	writeq(OStream, Option1),
	write(OStream, '.\n'),
	write_model_options(OStream,Rest).


%%%
% Write the input formats of a Model called with Goal to the file OutputFile
%
lost_model_input_formats_to_file(Model,Goal,OutputFile) :-
	lost_interface_input_formats(Model,Goal,Formats),
	open(OutputFile,write,OStream),
	write(OStream,lost_input_formats(Model,Goal,Formats)),
	write(OStream, '.\n'),
	close(OStream).


/*lost_model_output_format_to_file(Model,Goal,Options,OutputFile) :-
	lost_interface_output_format(Model,Goal,Option,OutputFormat),
	open(OutputFile,write,OStream),
	write(OStream,lost_model_output_format(Model,Goal,Options,OutputFormat)),
	write(OStream, '.\n'),
	close(OStream).
*/	
