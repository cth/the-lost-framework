% Interface file for the genemark module
:- ['../../lost.pl'].


% Load configuration options
:- [config].
:- [ldata_parser].

:- lost_include_api(interface).

% Load configuration options
% Load configuration options
% InputFile: Should be a file in fasta format
lost_best_annotation([InputFile], Options, OutputFile) :-
      lost_required_option(Options,parameters, ParamsId),
      genemark_parameter_file(ParamsId,ParamsFile),
      write(ParamsFile),nl,
      genemark_setup(InputFile,GMInputFile),
      genemark_run(GMInputFile,ParamsFile),
      atom_concat(GMInputFile,'.ldata',LDATAFILE),
      write('Parsing Genemark report...'),nl,
      ldata_parser_main(LDATAFILE,OutputFile),
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
	(file_exists(GeneMarkOutput1) ->
	 delete_file(GeneMarkOutput1) ; true),
	(file_exists(GeneMarkOutput2) ->
	 delete_file(GeneMarkOutput2) ; true),
	(file_exists(GeneMarkOutput3) ->
	 delete_file(GeneMarkOutput3) ; true).

%testme :-
%        lost_best_annotation(['/tmp/U00096_fna.seq'], 
%                             [option(parameters,'Escherichia_coli_K12')],
%                             'Test.out').

