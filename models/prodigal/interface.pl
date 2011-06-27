:- ['../../lost.pl'].

:- [config].
:- [glimmer3_parser].
:- lost_include_api(interface).

% Interface file for model that runs the Prodigal Gene finder 
% The annotate has the following required options,
% mode: (corresponds to glimmer3 scripts)
%    value=from-scratch: train using long-orfs and run prediction
%    value=from-training: traing using data specified by option parameter_file
%    value=iterated: ...

annotate([InputFile], Options, OutputFile) :-
        prodigal_config(prodigal-binary, Prodigal),
        lost_tmp_file(prodigal,TmpOutputFile),
        make_prodigal_command(Prodigal, InputFile, TmpOutputFile, Command),
        system(Command),
        parse_prodigal_file(TmpOutputFile,OutputFile).
                
prodigal_command(Executable,InputFile,OutputFile,Command) :-
        atom_concat_list(['cat ', InputFile, '|', Executable, ' > ', OutputFile ], Command). 
