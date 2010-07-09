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


%----
% logodds computation
%----

% Note: Full path of InputFile should be given
logodds_computation(InputFile,NameModel,NameNull,OptionsModel,OptionsNull,OutputFile) :-
        run_model(NameModel,annotate([InputFile],OptionsModel,OutputFile_Model)),
        run_model(NameNull,annotate([InputFile],OptionsNull,OutputFile_Null)),
        run_model(logodds,annotate([OutputFile_Model,OutputFile_Null],[],OutputFile)).
                  
