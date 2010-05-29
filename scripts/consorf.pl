% This script is for the toplogy of a the consorf genefinder relying on annotations from
% open reading frame annotator and conservation annotator.
% The consorf genefinder is based on a paired hidden markov model that should be properly parameterized.
% While the genefinder itself does not at this point care about what chunks it predicts, the inout files must
% reflect the same chunking of data. As usual, we take care of that by including the toplogy of prediction models in this script.

:- ['../lost.pl'].

% Prolog files in the shared directory can be consulted
% like this from anywhere..
:- lost_include_api(interface).
:- lost_include_api(utils_parser_report).


% topology 

%--------------------------------------------------------------------------------------------------
% First the chunking. 
%--------------------------------------------------------------------------------------------------
% 	Here required options are:
%			frame(reading-frame) and 
%			direction(direction)?
%		
% 	OrfChunk_File consist of facts:
%			chunk(Id, Start, Stop, ChunkSequence, Dir Frame, List_of_potenital_start_codons, List_of_potential_stop_codons).
%--------------------------------------------------------------------------------------------------
run_orf_chopper(Sequence_File,Options,OrfChunk_File) :-
	lost_sequence_file(Sequence_File,Sequence),
	get_annotation_file('orf_chopper',
			    [Sequence],
			    Options,
			    OrfChunk_File).
			    
%--------------------------------------------------------------------------------------------------
% Translation to amino-acids. 
%--------------------------------------------------------------------------------------------------
% 	Required options ase:
%			mode(translation_mode)
%		
%		where translation_mode = 0 means entire chunk is translated, 
%		and translation_mode = 1 means only longest orf is translated
%
%   Translated_Chunk_File consist of translated chunks in fastaformat:
%			....explanation
%		
%--------------------------------------------------------------------------------------------------
run_chunk_translator(Sequence_File,Options,Translated_Chunk_File) :-
	run_orf_chopper(Sequence_File,Options,Orf_Chunk_File),
	get_annotation_file(chunk_translator,
			    [Orf_Chunk_File],
			    Options,
			    Translated_Chunk_File).			    


%--------------------------------------------------------------------------------------------------
% Computing conservation annotation
%--------------------------------------------------------------------------------------------------
% 	Possible options include scoring mode for non-gap mismatches : nmScore(Score)
%			Score = 0, nongap-mismatches score nothing, 
%			Score = 1, nongap-mismathces score fully
%
%		and an optional : alignment outputfile : alignments(Alnfile)
% 	also direction and frame are needed
%--------------------------------------------------------------------------------------------------
run_chunk_conservation(Sequence_File,Options,Conservation_File) :-
	run_chunk_translator(Sequence_File,Options,Translated_Chunk_File),
	get_annotation_file(chunk_aa_conservation,
			    [Translated_Chunk_File],
			    Options,
			    Conservation_File).

%-------------
% Run orf annotator
%-----------
% From the sequence file, generation of the orf annotation from the result
% of the orf chopper:
%     orf_annotation(Key,Left,Right,Dir,Frame,[seq_annot([Annot])]
% where Annot = [.,.,.,.,.,<,<,<,-,-,-,-,>,>,>]
%-----------
run_orf_annotator(Sequence_File,Options,Orf_annot_File) :-
        run_orf_chopper(Sequence_File,Options,Chunk_File),
        get_annotation_file(orf_annotator,
                            [Chunk_File],
                            [],
                            Orf_annot_File).


%--------------------------------------------------------------------------------------------------
% driving run-predicate for testing and debugging
%--------------------------------------------------------------------------------------------------
consorf(InputOrfFile,InputConsFile):-
	lost_sequence_file(InputOrfFile,LostInputOrfFile), % ,ConsFile),
	lost_sequence_file(InputConsFile,LostInputConsFile),
	lost_model_parameter_file(consorf_genefinder, consorf_genefinder, ParameterFile),	
	get_annotation_file(consorf_genefinder,  					
			    [LostInputOrfFile,LostInputConsFile], 						
			    [parameter_file(ParameterFile)],   
			    AnnotFile),
	write('Resulting consorf prediction file'),nl,
	writeln(AnnotFile).

%--------------------------------------------------------------------------------------------------
% driving run-predicate once required models have been ported to LoSt framework
%--------------------------------------------------------------------------------------------------
run_consorf(Sequence_File,Options,Prediction_File):-
  run_orf_annotator(Sequence_File,Options,Input_Orf_File),
	run_chunk_conservation(Sequence_File,Options,Input_Cons_File),
	lost_model_parameter_file(consorf_genefinder, consorf_genefinder, ParameterFile),
	get_annotation_file(consorf_genefinder,  					
			    [Input_Orf_File,Input_Cons_File], 						
			    [option(parameter_file,ParameterFile)],   
			    Prediction_File),     													
	write('Resulting consorf prediction file'),nl,
	writeln(Prediction_File).



% test predicate

testgoal:-run_chunk_conservation('U00096-500',[direction(+),frame(1),mode(0),nmScore(1),genecodefile('genecode11.pl')],Output), write('Output :'),writeln(Output).				



