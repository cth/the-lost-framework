% This script file defined several script to play with the Hein HMM

:- lost_include_script(script_parser).

% Topology:
% model(fna_parser,*.fna,Nucleotids_File)
% model(hein_genefinder,Nucleotids_File

run_hein :-
        parser_fna('U00096_fna','U00096_gbk',[list(280)],InputFile),
        run_model(hein_genefinder, % Name of Model
                  annotate([InputFile], % DB data
                           [range(1,2000)], % Options
                           OutputFile)),
        write("Hein analysis succeeds!! Results store in: "),
        write(OutputFile).
