:- module(filepath,[]).

/** <module> filepath 
@author Christian Theil Have

Directory and file management in the LoSt framework.

*/

%% lost_testcase_directory(-TestCaseDir) is det
% TestCaseDir is the absolute path to the directory containing test cases:
% ==
% $LOST_BASE_DIR/test/
% ==
lost_testcase_directory(TestCaseDir) :-
	lost_config(lost_base_directory,BaseDir),!,
	atom_concat(BaseDir,'/test/', TestCaseDir).


%% lost_models_directory(-TmpDir) is det
% TmpDir is the absolute path to the directory containing temporary files:
% ==
% $LOST_BASE_DIR/tmp/
% ==
lost_tmp_directory(TmpDir) :-
        lost_config(lost_base_directory,BaseDir),!,
        atom_concat(BaseDir,'/tmp/', TmpDir).

%% lost_utilities_directory(-Dir) is det
% Dir is the absolutate path to the directory containing utility scripts
% == 
% $LOST_BASE_DIR/utilities/
% ==
lost_utilities_directory(UtilsDir) :-
        lost_config(lost_base_directory,BaseDir),!,
        atom_concat(BaseDir,'/utilities/', UtilsDir).

%% lost_models_directory(-ModelsDir) is det
% ModelsDir is absolutate path to the directory containing models:
% ==
% $LOST_BASE_DIR/models/
% ==
lost_models_directory(ModelsDir) :-
	lost_config(lost_base_directory, Basedir),!,
	atom_concat(Basedir, '/models/', ModelsDir).

%% lost_data_directory(-Dir) is det
% Dir is the absolutate path to the directory containing data files
% == 
% $LOST_BASE_DIR/data/
% ==
lost_data_directory(Dir) :-
	lost_config(lost_base_directory, Basedir),!,
	atom_concat(Basedir,'/data/',Dir).
	
%% lost_script_directory(-Dir) is det
% Dir is the absolutate path to the directory containing LoSt scripts
% == 
% $LOST_BASE_DIR/scripts/
% ==
lost_script_directory(Dir) :-
	lost_config(lost_base_directory, Basedir),!,
	atom_concat(Basedir,'/scripts/',Dir).


%% lost_model_directory(+Model,-ModelDir) is det
% ModelDir is the absolute path to the directory containing the the model with name Model
lost_model_directory(Model,ModelDir) :-
	lost_models_directory(ModelsDir),
	atom_concat(ModelsDir,Model,ModelDir1),
	atom_concat(ModelDir1,'/',ModelDir).
	
%% lost_model_interface_file(+Model,-ModelFile) is det
% ModelFile is the absolute path the 'interface.pl' file for the model with name Model
lost_model_interface_file(Model,ModelFile) :-
	lost_model_directory(Model,ModelDir),
	atom_concat(ModelDir, 'interface.pl',ModelFile).

%% lost_data_index_file(-IndexFile)
% IndexFile is absolute path to the database containing the an index of Lost script result files
lost_data_index_file(IndexFile) :-
	lost_data_directory(AnnotDir),
	atom_concat(AnnotDir,'annotations.idx',IndexFile).

%%% lost_data_file(+SequenceId, -SequenceFile)
% 
%lost_sequence_file(SequenceId, SequenceFile) :-
%	lost_data_directory(D),
%	atom_concat(SequenceId,'.seq', Filename),
%	atom_concat(D, Filename, SequenceFile).

% lost_data_file(+BaseFileName,-AbsoluteFileName) is det
% AbsoluteFileName is the absolute file name of a file in the $LOST_BASE_DIR/data/ directory with the name BaseFileName
lost_data_file(Filename, DataFile) :-
	lost_data_directory(D),
	atom_concat(D, Filename, DataFile).
	
%% lost_tmp_file(+Prefix, -TmpFile) is det
% Creates a temporary file in the $LOST_TMP_DIR directory with a unique name starting with Prefix
lost_tmp_file(Prefix,TmpFile) :-
        random_uniform(X), % prism specific
        Y is X * 65536,
        SuffixInt is round(Y),
        atom_integer(Suffix,SuffixInt),
        lost_tmp_directory(TmpDir),
        atom_concat_list([TmpDir, '/' , Prefix, '-', Suffix],TmpFile).
