% Several scripts to test and use the different models
% of S�ren

:- ['../lost.pl'].


%----
% Unit test
%----


% test model

test_model(NameModel,Options,O) :-
        lost_test_file('DT_soer_models',InputFile),
        run_model(NameModel,annotate([InputFile],Options,OutputFile)). 
