:- ['../../lost.pl'].

:- [config].
:- [parser].
:- lost_include_api(interface).

% Interface file for model that runs the Prodigal Gene finder 

annotate([InputFile], Options, OutputFile) :-
        prodigal_config(prodigal-binary, Prodigal),
        lost_tmp_file(prodigal,TmpOutputFile),
        make_prodigal_command(Prodigal, InputFile, TmpOutputFile, Command),
        system(Command),
        parse_prodigal_file(TmpOutputFile,OutputFile).
                
prodigal_command(Executable,InputFile,OutputFile,Command) :-
        atom_concat_list(['cat ', InputFile, '|', Executable, ' > ', OutputFile ], Command). 
