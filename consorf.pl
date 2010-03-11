% This example illustrate how we use get_annotation to 
% have models run a files generated automatically

:- [lost].

% Prolog files in the shared directory can be consulted
% like this from anywhere..
:- lost_include_api(interface).



consorf:-
	% Unify InputSeqFile to the full filename of tinytest e.g. .../sequences/tinytest.seq
	lost_sequence_file('u00096-20k_orf_+1',LostInputOrfFile), % 
	lost_sequence_file('u00096-20k_cns0_+1',LostInputConsFile),
	
	% Parameter id "sample2" resolves to models/consorf_genefinder/parameters/consorf_genefinder.prb
	lost_model_parameter_file(consorf_genefinder, consorf_genefinder, ParameterFile),
	
	get_annotation_file(consorf_genefinder,  					
			    [LostInputOrfFile,LostInputConsFile], 						
			    [option(parameter_file,ParameterFile)],   
			    AnnotFile),     													

	write('Resulting consorf prediction file'),nl,
	writeln(AnnotFile).


/*
consorf(InputOrfFile,InputConsFile):-
	% Unify InputSeqFile to the full filename of tinytest e.g. .../sequences/tinytest.seq
	lost_sequence_file(InputOrfFile,LostInputOrfFile), % ,ConsFile),
	lost_sequence_file(InputConsFile,LostInputConsFile),
	% Parameter id "sample2" resolves to models/consorf_genefinder/parameters/consorf_genefinder.prb
	lost_model_parameter_file(consorf_genefinder, consorf_genefinder, ParameterFile),
	
	get_annotation_file(consorf_genefinder,  					
			    [LostInputOrfFile,LostInputConsFile], 						
			    [option(parameter_file,ParameterFile)],   
			    AnnotFile),     													

	write('Resulting consorf prediction file'),nl,
	writeln(AnnotFile).
*/