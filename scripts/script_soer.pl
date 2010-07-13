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


% Dedicated to E.COli
data_learning(OutputFile) :-
        lost_sequence_file('U00096_ptt',PTTFile),
        run_model(parser_ptt,annotate([PTTFile],[],ParsedPTT)),
        Options = [regex_no_match_extra_fields([
			product("^.*(predicted|putative|unknown|possible|hypothetical|probable|bacteriophage|transposon|insertion|reverse transcriptase).*$")
                                               ])
                  ],
        lost_sequence_file('U00096',RawGenome),
        run_model(gene_filter,annotate([ParsedPTT,RawGenome],Options,OutputFile)).
        




%----
% Data Prediction
%----
