%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Configuration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lost_config(prism_command,'/opt/prism/bin/prism').
% launch_prism_process('/tmp/test.pl','hello').
% cd('/home/cth/code/lost/shared').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Launching a PRISM process
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

launch_prism_process(PrismPrologFile, Goal) :-
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
	sequence_terms_to_elements(SortedTerms,Sequence).

save_sequence_list_to_file(File,Sequence) :-
	data_elements_to_sequence_terms(Sequence,Terms),
	term_to_file(File,Terms).

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
	basedir(Basedir),
	atom_concat(Basedir, '/models/', ModelsDir).

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
	atom_concat(Dir,ParameterId,ParameterFile).

% The naming of files!

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lost model interface analysis
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
	write_terms_to_stream(Rest).


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

lost_interface_supports(Model,Functor,Arity) :-
	lost_model_interface_file(Model,ModelFile),
	lost_parse_interface_file(ModelFile, Terms),
	terms_has_rule_with_head(Terms,Functor,Arity).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% File name management
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%lost_parameter_filename(

lost_annotation_filename(Model,ParamsId,InputFiles,Filename) :-
	sort('=<',InputFiles,SortedInputFiles),
	% builtin hash_code/2 might to simple and prone to collisions.
	% It maps to a (not so big) integer value..
	% Maybe it should be replaced with something more safe later on.
	hash_code(SortedInputFiles,HashValue),
	lost_model_annotations_directory(Model,Dir),
	atom_concat(Dir, ParamsId, FName1),
	atom_concat(FName1, '_', FName2),
	atom_concat(FName2, HashValue, FName3),
	atom_concat(FName3, '.seq',Filename).

			
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Will unify Filename with the name of the containing an annotation
% the the prism run with Model, Params and Inputs. If no such annotation
% exists, PRISM will be run to generate it.
% Model: The name of the model to be run
% Params: Identifier for the parameter file.
% Inputs: A list of filenames given as input to the model.
get_annotation_file(Model, Params, Inputs, Filename) :-
	lost_model_interface_file(Model,ModelFile),
	lost_model_parameter_file(Model,ParamFile),
	(file_exists(ModelFile) ; throw(interface_error(missing_model_file(ModelFile)))),
	(file_exists(ParamFile) ; throw(interface_error(missing_param_file(ParamFile)))),
	(check_interface_supports(Model,lost_best_annotation,3) ;
	 throw(interface_error(no_support(Model,lost_best_annotation/3)))),
	% Check if a result allready exists:
	lost_annotation_filename(Model,Params,Inputs,Filename),
	(file_exists(Filename) ;
	 term2atom(lost_best_annotation(ParamFile,Inputs,Filename),Goal),
	 launch_prism_process_blocking(ModelFile,Goal)
	),
	(file_exist(Filename); throw(interface_error(missing_annotation_file(Filename)))).
	
get_annotation_data(AnnotFile,Annot) :-
	load_sequence_list_from_file(AnnotFile,Annot).


