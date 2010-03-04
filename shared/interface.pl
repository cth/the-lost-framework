%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get_annotation_file/4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Will unify Filename with the name of the containing an annotation
% the the prism run with Model, Params and Inputs. If no such annotation
% exists, PRISM will be run to generate it.
% Model: The name of the model to be run
% Params: Identifier for the parameter file.
% Inputs: A list of filenames given as input to the model.
get_annotation_file(Model, ParametersId, Inputs, Filename) :-
	lost_model_interface_file(Model, ModelFile),
	lost_model_parameter_file(Model,ParametersId,ParamFile),
	(file_exists(ModelFile) ; throw(interface_error(missing_model_file(ModelFile)))),
	(file_exists(ParamFile) ; throw(interface_error(missing_param_file(ParamFile)))),
	(lost_interface_supports(Model,lost_best_annotation,3) ;
	 throw(interface_error(no_support(Model,lost_best_annotation/3)))),
	% Check if a result allready exists:
	lost_annotation_file(Model,ParametersId,Inputs,Filename),
	write(lost_annotation_file(Model,ParametersId,Inputs,Filename)),nl,
	(file_exists(Filename) ;
	 term2atom(lost_best_annotation(ParamFile,Inputs,Filename),Goal),
	 launch_prism_process(ModelFile,Goal)
	),
	(file_exists(Filename); throw(interface_error(missing_annotation_file(Filename)))).
	
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
	atom_concat(PRISM, ' -g "cl(\'', Cmd0),
	atom_concat(Cmd0, Filename, Cmd1),
	atom_concat(Cmd1,'\'), ',Cmd2),
	atom_concat(Cmd2, Goal, Cmd3),
	atom_concat(Cmd3,'"',Cmd4),
	write('cmd: '), write(Cmd4),nl,
	% FIXME: Setup some stdout redirection (this may be troublesome on windows)
	% Run PRISM
	system(Cmd4,ExitCode),
	write(ExitCode),nl,
	% Unfortunately bprolog/prism does get the concept of exit codes. DOH!!!
	(ExitCode == 0 ; throw(error(launch_prism_process(PrismPrologFile,Goal)))),
	chdir(CurrentDir).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Rudimentary management of sequence data 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Asssuming that the file contains facts on the form:
% elem(1,...), elem(2,...) etc.
load_sequence_list_from_file(File,Sequence) :-
	terms_from_file(File,Terms),
	% Technically not necessary since they will be sorted if this
	% interface is used
	sort(Terms,SortedTerms), 
	sequence_terms_to_data_elements(SortedTerms,Sequence).

save_sequence_list_to_file(File,Sequence) :-
	data_elements_to_sequence_terms(Sequence,Terms),
	terms_to_file(File,Terms).

sequence_terms_to_data_elements([],[]).
sequence_terms_to_data_elements([elem(_,Data)|R1],[Data|R2]) :-
	sequence_terms_to_data_elements(R1,R2).


data_elements_to_sequence_terms(Data,Terms) :-
	data_elements_to_sequence_terms(1,Data,Terms).

data_elements_to_sequence_terms(_,[],[]).
data_elements_to_sequence_terms(Pos,[Data|R1],[elem(Pos,Data)|R2]) :-
	NextPos is Pos + 1,
	data_elements_to_sequence_terms(NextPos,R1,R2).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Directory and file management
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lost_models_directory(ModelsDir) :-
	lost_config(lost_base_directory, Basedir),
	atom_concat(Basedir, '/models/', ModelsDir).

lost_sequence_directory(Dir) :-
	lost_config(lost_base_directory, Basedir),
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

lost_model_parameter_file(Model,ParameterId,ParameterFile) :-
	lost_model_parameters_directory(Model,Dir),
	atom_concat(Dir,ParameterId,ParameterFile1),
	atom_concat(ParameterFile1,'.prb',ParameterFile).

lost_annotation_file(Model,ParamsId,InputFiles,Filename) :-
	lost_sequence_directory(Model,AnnotDir),
	lost_annotation_index_file(IndexFile),
	lost_aidx_get_filename(IndexFile,Model,ParamsId,InputFiles,Filename).

lost_sequence_file(SequenceId, SequenceFile) :-
	lost_sequence_directory(D),
	atom_concat(SequenceId,'.seq', Filename),
	atom_concat(D, Filename, SequenceFile).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lost annotation index.
% rule prefix: lost_aidx_
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The index file contains a list of facts on the form
% fileid(FileId,Filename,Model,ParametersId,InputFiles).
% where InputFiles is a list.

% Retrieve the filename matching (Model,ParamsId,InputFiles) from the annotation index
% If no such filename exists in the index, then a new unique filename is created
% and unified to Filename 
lost_aidx_get_filename(IndexFile,Model,ParamsId,InputFiles,Filename) :-
	(file_exists(IndexFile) ; lost_aidx_create_index_file(IndexFile)),
	terms_from_file(IndexFile,Terms),
	(lost_aidx_get_filename_from_terms(Terms,Model,ParamsId,InputFiles,Filename) ->
	 ;
	 lost_aidx_next_available_index(Terms, Index),
	 atom_concat_list([AnnotDir, Model, '_annot_',Index,'.seq'], Filename),
	 append(Terms, [fileid(Index,Filename,Model,ParamsId,InputFiles)],NewTerms),
	 terms_to_file(IndexFile,NewTerms)
	).

% Given Terms, unify NextAvailableIndex with a unique index not
% occuring as index in  terms:
lost_aidx_get_next_available_index(Terms, NextAvailableIndex) :-
	annotation_get_largest_index(Terms,LargestIndex),
	NextAvailableIndex is LargestIndex + 1.

% Unify second argument with largest index occuring in terms
lost_aidx_get_largest_index([], 0).
lost_aidx_get_largest_index([Term|Rest], LargestIndex) :-
	Term =.. [ fileid, TermIndex | _ ],
	annotation_get_next_available_index(Rest,MaxRestIndex),
	max(TermIndex,MaxRestIndex,LargestIndex).

% lost_aidx_get_filename_from_terms/5:
% Go through a list terms and check find a Filename matching (Model,ParamsId,InputFiles)
% Fail if no such term exist
lost_aidx_get_filename_from_terms([Term|_],Model,ParamsId,InputFiles,Filename) :-
	Term =.. [ fileid, _, Filename, Model, ParamsId, InputFiles ].

lost_aidx_get_filename_from_terms([_|Rest],Model,ParamsId,InputFiles,Filename) :-
	lost_aidx_get_filename_from_terms(Rest,Model,ParamsId,InputFiles,Filename).

	
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