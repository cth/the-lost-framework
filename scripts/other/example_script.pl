% This example illustrate how we use get_annotation to 
% have models run a files generated automatically

model2_params <- sample_model2::train(lost_data_file(tinytest)).

model1_params <- lost_data_file('tiny-params').

tinyseq <- lost_data_file('tinytest.seq').

%refseq_nc00913 <- http::get(['http://ncbi/genomes/ecoli'])

% CHR like rules:
% annotation1, annotation2, model3(annotation1, annotation2) ==> annotation3.

annotation1 <- write('simple guard') | sample_model1::annotate([tinyseq,model1_params]).

annotation2 <- write('composite'), write(' guard') | sample_model2::annotate([tinyseq,annotation1,model2_params], [use_parameter_file(no)]).

%annotation(X) <- model::annotate([X]).

% ?- run(annotation(file1))

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