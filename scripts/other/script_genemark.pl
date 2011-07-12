% This script file defined several script to play with the Genemark.HMM
:- ['../lost.pl'].
:- lost_include_api(interface).
:- lost_include_api(autoAnnotations).
:- lost_include_script(script_parser).

% Topology:
% model(fna_parser,*.fna,Nucleotids_File)
% model(hein_genefinder,Nucleotids_File

run_genemark(FnaFile,GbkFile,[Min,Max],OutputFile) :-
        parser_fna(FnaFile,GbkFile,[],InputFile),
        run_model(genemark_genefinder, % Name of Model
                  annotate([InputFile], % DB data
                           [range(Min,Max)], % Options
                            OutputFile)),
        write("Genemark analysis succeeds!! Results store in: "),
        write(OutputFile).
