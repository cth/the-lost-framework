:- use(interface).

:- [config].
:- [parser].


:- task(parse([text(glimmer3_report)]),[],text(prolog(ranges(gene)))).

% Interface file for model that runs Glimmer3

%% parse(+InputFiles,+Options,+OutputFile)
% Parse a Glimmer 3 report file.
parse([InputFile],_,OutputFile) :-
	writeln(parse_glimmer_file),
	parse_glimmer_file(InputFile,OutputFile).

%% annotate(+InputFiles,+Options,+OutputFile)
% ==
% InputFiles = [ InputFile, TrainingDataInputFile ]
% ==
% The =|mode|= (corresponds to glimmer3 scripts)
%    value=from-scratch: train using =|long-orfs|= and run prediction. Long-orfs is a glimmer specific program that extracts long orfs.
%    value=from-training: traing using data specified by option parameter_file
%    value=iterated: ...
annotate([InputFile,TrainingInputFile], Options, OutputFile) :-
  get_option(Options,mode,from-scratch),
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

% Given a list of Orfs,
score_orfs([OrfFile, FastaFile], Options, OutputFile) :-
	lost_tmp_directory(Tmp),
	atom_concat(Tmp,'glimmer3_format.orfs',GlimmerOrfsFile),
	terms_from_file(OrfFile,Orfs),
	tell(GlimmerOrfsFile),
	write_orfs_in_glimmer3_format(Orfs),
	told.

/*
write_orfs_in_glimmer3_format([Orf|Orfs]) :-
	Orf =.. [ _func, _id, Start, Stop, 
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Uses the g3-from-scratch script to run glimmer in from scratch mode.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Expects a fasta input file
g3_from_scratch([FastaInputFile],_Options,OutputFile) :-
  glimmer3_config(scripts_directory, ScriptsDir),
  atom_concat_list([ScriptsDir,'g3-from-scratch.csh'], Script),
  lost_tmp_directory(TmpDir),
  atom_concat(TmpDir, 'glimmer3', OutputFileTag),
  atom_concat_list([Script, ' ', FastaInputFile, ' ', OutputFileTag], ScriptCmd),
  write('running glimmer3:'),nl,
  writeq(ScriptCmd),nl,
  system(ScriptCmd),
  atom_concat(OutputFileTag,'.predict',PredictionFile),
  glimmer3_parse_prediction_file(PredictionFile,OutputFile).	

