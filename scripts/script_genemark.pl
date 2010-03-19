% This script file defined several script to play with the Hein HMM
:- ['../lost.pl'].
:- lost_include_api(interface).
:- lost_include_api(autoAnnotations).
:- lost_include_script(script_parser).

% Topology:
% model(fna_parser,*.fna,Nucleotids_File)
% model(hein_genefinder,Nucleotids_File

run_genemark(FnaFile,GbkFile,[Min,Max],OutputFile) :-
        parser_fna(FnaFile,GbkFile,[list(280)],InputFile),
        get_annotation_file(genemark_genefinder, % Name of Model
                            [InputFile], % DB data
                            [range(Min,Max)], % Options
                             OutputFile
                           ),
        write("Hein analysis succeeds!! Results store in: "),
        write(OutputFile).
