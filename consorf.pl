% This example illustrate how we use get_annotation to 
% have models run a files generated automatically

:- [lost].

% Prolog files in the shared directory can be consulted
% like this from anywhere..
:- lost_include_api(interface).

test_consorf :-
	% Unify InputSeqFile to the full filename of tinytest e.g. .../sequences/tinytest.seq
	writeln('foobar 1'),
	lost_sequence_file('u00096-20k_cnk_+1_tx0_cns',InputConsFile),
	writeln('foobar 2'),
	lost_sequence_file('u00096-20k_orf_+1',InputOrfFile), % ,ConsFile),
	writeln('foobar 3'),
	% Parameter id "sample2" resolves to models/consorf_genefinder/parameters/consorf_genefinder.prb
	writeln('foobar 4'),
	lost_model_parameter_file(consorf_genefinder, consorf_genefinder, ParameterFile),
	writeln('foobar 5'),
	get_annotation_file(consorf_genefinder,  					% Name of model (resolves to models/sample_model2/)
			    [InputOrfFile,InputConsFile], 						% A list of input files
			    [option(parameter_file,ParameterFile)],   % Extra options
			    AnnotFile),     														% AnnotFile is unified to the name of the file that  annotation is written to

																										% Load the sequence (AnnotSeq) contained in the file AnnotFile 
	
	writeln('foobar 6'),
	open(AnnotFile,read,Annots,[alias(annots)]),
	read_term(Annots,Term),
	write('Resulting annotation sequence:'),nl,
	writeln(Term),
	close(Annots)
	.


