% This example illustrate how we use get_annotation to 
% have models run a files generated automatically

:- [lost].

% Prolog files in the shared directory can be consulted
% like this from anywhere..
:- lost_include_api(interface).

test :-
	% Unify InputSeqFile to the full filename of tinytest e.g. .../sequences/tinytest.seq
	lost_sequence_file(tinytest,InputSeqFile),
	
	get_annotation_file(sample_model2,  % Name of model (resolves to models/sample_model2/)
			    sample2,        % Parameter Id (resolves to models/sample_model2/parameters/sample2.prb)
			    [InputSeqFile], % A list of input files
			    [],             % Extra options
			    AnnotFile),     % AnnotFile is unified to the name of the file that  annotation is written to

	% Load the sequence (AnnotSeq) contained in the file AnnotFile 
	load_sequence_list_from_file(AnnotFile,AnnotSeq),
	
	write('Resulting annotation sequence:'),nl,
	write(AnnotSeq),nl.

