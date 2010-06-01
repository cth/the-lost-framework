% Interface file for the genemark module
:- ['../../lost.pl'].


% Load configuration options
:- [config].
:- [ldata_parser].

:- lost_include_api(interface).
:- lost_include_api(misc_utils).

lost_option(annotate,parameters,'Escherichia_coli_K12','The named parameter set to use for prediction. Should be similar to organism of the sequence').

lost_option_values(annotate,parameters,Values) :-
	genemark_config(genemark_parameters_dir,GMDir),
	directory_files(GMDir,DirFileAtoms),
	map(atom_codes, DirFileAtoms, DirFileCodes),
	atom_codes('.mat',ExtensionCode),
	findall(Head, (member(DirFileCode,DirFileCodes),match_tail(DirFileCode,Head,ExtensionCode)), ValuesCodes),
	map(atom_codes(output,input), ValuesCodes, Values).
	
lost_input_formats(annotate, [text(fasta(fna))]).
lost_output_format(annotate, _, text(prolog(ranges(gene)))).

% Load configuration options
% Load configuration options
% InputFile: Should be a file in fasta format
annotate([InputFile], Options, OutputFile) :-
      get_option(Options,parameters, ParamsId),
      genemark_parameter_file(ParamsId,ParamsFile),
      write(ParamsFile),nl,
      genemark_setup(InputFile,GMInputFile),
      genemark_run(GMInputFile,ParamsFile),
      atom_concat(GMInputFile,'.ldata',LDATAFILE),
      write('Parsing Genemark report...'),nl,
%      atom_concat(LDATAFILE,'.parsed.pl',ParsedLDataFile),
      ldata_parser_main(LDATAFILE,OutputFile),
%      genemark_include_best_predictions(ParsedLDataFile,OutputFile),
      genemark_cleanup(GMInputFile).

genemark_parameter_file(ParamsId,ParamsFile) :-
        genemark_config(genemark_parameters_dir,GMDir),
        atom_concat_list([GMDir,ParamsId,'.mat'],ParamsFile).
         
genemark_run(InputFile,ParamsFile) :-
        genemark_config(genemark_command,GMCmd),
        atom_concat_list([ GMCmd, ' -D -m ', ParamsFile, ' ', InputFile],FullCommand),
        write('Running genemark: '),nl,
        writeq(FullCommand),nl,
        system(FullCommand).
                
genemark_setup(InputFile,GenemarkInputFile) :-
        lost_tmp_directory(TmpDir),
        atom_concat(TmpDir,'genemark',GenemarkInputFile),
        copy_file(InputFile,GenemarkInputFile).

genemark_cleanup(GMInputFile) :-
        atom_concat(GMInputFile,'.lst',GeneMarkOutput1),
        atom_concat(GMInputFile,'.ldata',GeneMarkOutput2),
        atom_concat(GMInputFile,'.gdata',GeneMarkOutput3),
	!,
	(file_exists(GeneMarkOutput1) ->
	 delete_file(GeneMarkOutput1) ; true),
	(file_exists(GeneMarkOutput2) ->
	 delete_file(GeneMarkOutput2) ; true),
	(file_exists(GeneMarkOutput3) ->
	 delete_file(GeneMarkOutput3) ; true).

%testme :-
%        annotate(['/tmp/U00096_fna.seq'], 
%                             [option(parameters,'Escherichia_coli_K12')],
%                             'Test.out').
