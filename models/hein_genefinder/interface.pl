:- ['../../lost.pl'].                                                                   
:- lost_include_api(interface).                                                         
:- lost_include_api(autoAnnotations).
:- lost_include_api(misc_utils).
:- lost_include_api(io).                                                                                                                                        
                                                                                        
% 

lost_best_annotation([InputFile],Options,OutputFile) :-                                 
	write('Hein genefinder: '),nl,                                                         
        prismAnnot('hein'), % Load the actual PRISM model                                         
        (lost_option(Options,parameter_file,ParamFile) ->
            restore_sw(ParamFile) % Restore switch values
            ;
            true
        ),
        % Building of the InputSeq
        (member(range(Min,Max),Options) -> % Warning: Range format (Min,Max)
            load_annotation_from_file(sequence,[data_position(4),range(Min,Max)],InputFile,InputSeq) % Warning: It assumes that the data list is in position 4 of terms
        ;
            load_annotation_from_file(sequence,[data_position(4)],InputFile,InputSeq)
        ),
        write(InputSeq),nl,
        write(viterbiAnnot(hein(InputSeq,Annotation),_P)),nl,
        check_or_fail(viterbiAnnot(hein(InputSeq,Annotation),_P),
                      error('Viterbi computation failed (Hein Model)')
                     ),
        save_annotation_to_sequence_file(hein_genefinder,70,Annotation,OutputFile).
				
				
	
