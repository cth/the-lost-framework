:- ['../../lost.pl'].

:- [config].
:- [glimmer3_parser].
:- lost_include_api(interface).

% Interface file for model that runs Glimmer3

% The lost_best_annotation has the following required options,
% mode: (corresponds to glimmer3 scripts)
%    value=from-scratch: train using long-orfs and run prediction
%    value=from-training: traing using data specified by option parameter_file
%    value=iterated: ...

lost_best_annotation([InputFile], Options, OutputFile) :-
  lost_required_option(Options,mode,from-scratch),
  glimmer3_config(scripts_directory, ScriptsDir),
  atom_concat_list([ScriptsDir,'g3-from-scratch.csh'], Script),
  lost_tmp_directory(TmpDir),
  atom_concat(TmpDir, 'glimmer3', OutputFileTag),
  atom_concat_list([Script, ' ', InputFile, ' ', OutputFileTag], ScriptCmd),
  write('running glimmer3:'),nl,
  writeq(ScriptCmd),nl,
  system(ScriptCmd),
  atom_concat(OutputFileTag,'.predict',PredictionFile),
  glimmer3_parse_prediction_file(PredictionFile,OutputFile).

%testme :-
%	lost_best_annotation(['/tmp/U00096_fna.seq'],
%			     [mode(from-scratch)],
%			     'test_out.pl').
  
