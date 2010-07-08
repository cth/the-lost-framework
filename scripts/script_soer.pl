% Several scripts to test and use the different models
% of Søren

:- ['../lost.pl'].


%----
% Unit test
%----


% iid model

test_iid_model(OutputFile) :-
        lost_sequence_file('DT_soer_models',InputFile),
        run_model(soer_iid,annotate([InputFile],[],OutputFile)).  % default usage of model: data_available = yes, data_functor = sequence
