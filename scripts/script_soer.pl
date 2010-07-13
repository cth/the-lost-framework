% Several scripts to test and use the different models
% of Søren

:- ['../lost.pl'].

%----
% Unit test
%----


% test model

test_model(NameModel,Options,OutputFile) :-
        lost_test_file('DT_soer_models',InputFile),
        run_model(NameModel,annotate([InputFile],Options,OutputFile)). 

test_logodds(OutputFile) :-
        lost_test_file('DT_soer_models',InputFile),
        logodds_computation(InputFile,ecoparse_adph,iid,[use_parameter_file(no)],[],OutputFile).

test_learn(NameModel,Options,OutputFile) :-
    lost_test_file('DT_soer_models',InputFile),
    lost_sequence_file('U00096',RawGenome),
    run_model(NameModel,learn([InputFile,RawGenome],Options,OutputFile)).

test_learn_ecoparse_adph(OutputFile) :-
        data_learning(InputFile),
        lost_sequence_file('U00096',RawGenome),
        run_model(ecoparse_adph,learn([InputFile,RawGenome],[data_available(no),data_number(100)],OutputFile)).

%----
% logodds computation
%----

% Note: Full path of InputFile should be given
logodds_computation(InputFile,NameModel,NameNull,OptionsModel,OptionsNull,OutputFile) :-
        run_model(NameModel,annotate([InputFile],OptionsModel,OutputFile_Model)),
        run_model(NameNull,annotate([InputFile],OptionsNull,OutputFile_Null)),
        run_model(logodds,annotate([OutputFile_Model,OutputFile_Null],[],OutputFile)).
                  



%-----
% Data Learning
%-----


% Dedicated to E.Coli
data_learning(OutputFile) :-
        lost_sequence_file('U00096_ptt',PTTFile),
        run_model(parser_ptt,annotate([PTTFile],[],ParsedPTT)),
        Options = [match_strands([+]),   % To avoid the problem in get_data_from_files that works only for the direct strand
                   regex_no_match_extra_fields([
			product("^.*(predicted|putative|unknown|possible|hypothetical|probable|bacteriophage|transposon|insertion|reverse transcriptase).*$")
                                               ])
                  ],
        lost_sequence_file('U00096',RawGenome),
        run_model(gene_filter,annotate([ParsedPTT,RawGenome],Options,OutputFile)).
        

%----
% Data Prediction
%----

% Not exactely the good data
orf_stop_to_stop(OutputFile) :-
        lost_sequence_file('U00096',RawGenome),
        run_model(orf_chopper,annotate([RawGenome],[strand(+),frame(1),minimal_length(30)],ORF_Frame_1)),
        run_model(orf_chopper,annotate([RawGenome],[strand(+),frame(2),minimal_length(30)],ORF_Frame_2)),
        run_model(orf_chopper,annotate([RawGenome],[strand(+),frame(3),minimal_length(30)],ORF_Frame_3)),
        run_model(orf_chopper,annotate([RawGenome],[strand(-),frame(4),minimal_length(30)],ORF_Frame_4)),
        run_model(orf_chopper,annotate([RawGenome],[strand(-),frame(5),minimal_length(30)],ORF_Frame_5)),
        run_model(orf_chopper,annotate([RawGenome],[strand(-),frame(6),minimal_length(30)],ORF_Frame_6)),
        run_model(merge_files,annotate([ORF_Frame_1,ORF_Frame_2,ORF_Frame_3,ORF_Frame_4,ORF_Frame_5,ORF_Frame_6],[],OutputFile)).
