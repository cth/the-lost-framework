% This example illustrate how we use get_annotation to 
% have models run a files generated automatically

:- ['../lost.pl'].


%:- write('test'), op(1020, xfx, using).

% Prolog files in the shared directory can be consulted
% like this from anywhere..
:- lost_include_api(interface).
:- lost_include_api(io).

model1 <- 

run_model1(ResultFile) :-
	lost_sequence_file(tinytest,TinySequence),
	lost_model_parameter_file(sample_model1, test, Model1ParamFile),
	run_model(sample_model1, annotate([TinySequence,Model1ParamFile],[],ResultFile)).

run_model2(ResultFile) :-
	run_model1(AnnotModel1),
	lost_sequence_file(tinytest,TinySequence),
	lost_model_parameter_file(sample_model2, sample2, ParameterFile),
	run_model(sample_model2, annotate([TinySequence,AnnotModel1,ParameterFile],[use_parameter_file(no)],ResultFile)).

test_run :-
	run_model2(AnnotFile),
	write('Resulting annotation sequence file:'),nl,
	write(AnnotFile),nl,
	get_data_from_file(AnnotFile,[data_position(4)],AnnotSeq),
	write('Resulting annotation sequence:'),nl,
	write(AnnotSeq),nl.

test_train :-
	lost_sequence_file(tinytest,InputSeqFile),
	run_model(sample_model2, train([InputSeqFile],[],ModelParameterFile)),
	write('Resulting parameter file:'),nl,
	write(ModelParameterFile),nl.
