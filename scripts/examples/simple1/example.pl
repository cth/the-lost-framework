% This example illustrate how we use get_annotation to 
% have models run a files generated automatically

:- lost_include_api(interface).


% Initialization 
% Create two files: One with training data and one with the sequence that we are going to predict on
:-		lost_tmp_file('training_file',File1),
		assert(training_data_file(File1)),
		tell(File1),writeln('[a,g,a,g,a,a].'), told,
		lost_tmp_file('predict_file',File2),
		assert(prediction_data_file(File2)), 
		tell(File2), writeln('[a,g,a,g,a,a].'), told.

training_data <- training_data_file(F) | file::get(file(F)).
prediction_data <- prediction_data_file(F) | file::get(file(F)).

model1(parameters) <- sample_model1::train(training_data).

model1(prediction) <- sample_model1::annotate(prediction_data,model1(parameters)).

%model2(parameters) <- lost_sequence_file(tinytest,TinySequence) | sample_model2::annotate([TinySequence,])

/*
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
*/
