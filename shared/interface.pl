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
	check_or_fail(file_exists(ModelFile),interface_error(missing_model_file(ModelFile))),
	check_or_fail(lost_interface_supports(Model,lost_best_annotation,3),
		      interface_error(no_support(Model,lost_best_annotation/3))),
	% Check if a result allready exists:
	lost_annotation_index_file(AnnotIndex),
	lost_file_index_get_filename(AnnotIndex,Model,Inputs,Options,Filename),
	(file_exists(Filename) ->
	 write('Using existing annotation file: '), write(Filename),nl
	;
	 term2atom(lost_best_annotation(Inputs,Options,Filename),Goal),
	 launch_prism_process(ModelFile,Goal),
	 check_or_fail(file_exists(Filename),interface_error(missing_annotation_file(Filename))),
	 lost_file_index_update_file_timestamp(AnnotIndex,Filename)
	).

% (cth) Considering to rename "get_annotation_file" to "run_model"
run_model(Model,Inputs,Options,Filename) :-
	get_annotation_file(Model,Inputs,Options,Filename).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% train_model/4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

train_model(Model, TrainingDataFiles, Options, SavedParamsFile) :-
	lost_model_interface_file(Model, ModelFile),
	check_or_fail(file_exists(ModelFile),interface_error(missing_model_file(ModelFile))),
	check_or_fail(lost_interface_supports(Model,lost_best_annotation,3),
		      interface_error(no_support(Model,lost_learn/3))),
	lost_model_parameter_index_file(Model,ParamFileIndex),
	lost_file_index_get_filename(ParamFileIndex,Model,TrainingDataFiles,Options,SavedParamsFile),
	write(lost_file_index_get_filename(ParamFileIndex,Model,TrainingDataFiles,Options,SavedParamsFile)),nl,
	
	!,
	(file_exists(SavedParamsFile) ->
	 write('Using existing parameter file: '), write(SavedParamsFile), nl
	 ;
	 term2atom(lost_learn(TrainingDataFiles,Options,SavedParamsFile),Goal),
	 launch_prism_process(ModelFile,Goal),
	 check_or_fail(file_exists(SavedParamsFile),interface_error(missing_parameter_file(SavedParamsFile))),
	 write('about to update file timestamp'),nl,
	 lost_file_index_update_file_timestamp(ParamFileIndex,SavedParamsFile)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Option parsing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Get the Value of option with Key
lost_option([option(Key,Value)|_], Key, Value) :- !.
lost_option([option(_,_)|OptionList], Key, Value) :-
	lost_option(OptionList,Key,Value).

% Used to retrieve the value of required options
% Will throw expection if the value of the option cannot be found
lost_required_option(Options, Key, Value) :-
	check_or_fail(lost_option(Options,Key,Value),error(missing_option(Key))).
		    
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

lost_models_directory(ModelsDir) :-
	lost_config(lost_base_directory, Basedir),!,
	atom_concat(Basedir, '/models/', ModelsDir).

lost_sequence_directory(Dir) :-
	lost_config(lost_base_directory, Basedir),!,
	atom_concat(Basedir,'/sequences/',Dir).

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
	lost_sequence_directory(AnnotDir),
	atom_concat(AnnotDir,'annotations.idx',IndexFile).

lost_sequence_file(SequenceId, SequenceFile) :-
	lost_sequence_directory(D),
	atom_concat(SequenceId,'.seq', Filename),
	atom_concat(D, Filename, SequenceFile).

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
	 write(lost_file_index_next_available_index(Terms, Index)),nl,
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
lost_file_index_timestamp(timestamp(Year,Mon,Day,Hour,Min,Sec)) :-
	date(Year,Mon,Day),
	time(Hour,Min,Sec).

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
	nl,nl,writeq(UpdatedTerms),nl,nl,
	terms_to_file(IndexFile,UpdatedTerms).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lost model interface analysis
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lost_interface_supports(Model,Functor,Arity) :-
	lost_model_interface_file(Model,ModelFile),
	terms_from_file(ModelFile, Terms),
	terms_has_rule_with_head(Terms,Functor,Arity).
			
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Misc. utility rules used by the interface code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Parse an lost model interface file to a set of rules
terms_from_file(File, Terms) :-
	open(File, read, Stream),
	ground(Stream),
	collect_stream_terms(Stream,Terms),
	close(Stream).

terms_to_file(File,Terms) :-
	open(File,write,Stream),
	ground(Stream),
	write_terms_to_stream(Stream,Terms),
	close(Stream).

write_terms_to_stream(_,[]).
write_terms_to_stream(Stream,[Term|Rest]) :-
	writeq(Stream,Term),
	write(Stream,'.\n'),
	write_terms_to_stream(Stream,Rest).


% Create list of Rules found in Stream
collect_stream_terms(Stream, Rules) :-
	read(Stream, T),
	((T == end_of_file) ->
		Rules = []
	;
		collect_stream_terms(Stream,Rest),
		append([T],Rest,Rules)
	).

terms_has_rule_with_head(Terms,Functor,Arity) :-
	member(Rule, Terms),
	Rule =.. [ (:-), Head, _ ],
	functor(Head, Functor, Arity).

atom_concat_list([Atom],Atom).

atom_concat_list([Elem1,Elem2|Rest], CompositeAtom) :-
	atom_concat(Elem1,Elem2,Elem3),
	atom_concat_list([Elem3|Rest], CompositeAtom).

max(A,B,A) :- A > B.
max(A,B,B) :- A =< B.

atom2integer(Atom,Integer) :-
	atom_chars(Atom, Chars),
	number_chars(Integer, Chars).

check_or_fail(Check,_Error) :-
	call(Check),
	!.

check_or_fail(_File,Error) :-
	throw(Error).

dirname(Filename,DirPartAtom) :-
	% everything before last '/'=47 is dirname:
	atom_codes(Filename, CharCodes),
	append(DirPart, FilePart, CharCodes),
	append(_,[47],DirPart), % DirPart should end with a '/'
	not(member(47,FilePart)), 
	atom_codes(DirPartAtom,DirPart).
