:- ['../../lost.pl'].                                                                   
:- lost_include_api(interface).                                                         
:- lost_include(AutoAnnotations).                                                                                                                                          
                                                                                        
% This is what is used to get the best annotation                                       
lost_best_annotation([OrfFile,ConsFile],Options,OutputFile) :-                                 
				write('LoSt consorf genefinder: '),nl,                                                         
				write(lost_best_annotation(ParamFile,[OrfFile,ConsFile],Options,OutputFile)),nl,             
	
	lost_required_option(Options,parameter_file,ParamFile),                               
	prismAnnot('consorf_genefinder'), % Load the actual PRISM model                                         
	
				write(restore_sw(ParamFile)),nl, % Restore switch values                              
	
	restore_sw(ParamFile), % Restore switch values 
	                           
	load_annotation_from_file(sequence,[data_postion(6),range(1,48)],OrfFile,InputOrf), % Load the input sequence      
  load_annotation_from_file(sequence,[data_postion(6),range(1,48)],ConsFile,InputCons),      
        % Derive an annotation somehow
                                                          
  			write(viterbiAnnot(consorf(InputOrf,InputCons,OutputAnnotation))),nl,                      
	
	check_or_fail(viterbiAnnot(consorf(InputOrf,InputCons,OutputAnnotation))),                        
                        error(viterbiAnnot_says_no_no_no)),                                 
  
        write('LoSt consorf genefinder produced output prediction: '),                                    
        write(OutputAnnotation),nl,                                                       
				
	/*			
	write(save_sequence_list_to_file(OutputFile,OutputAnnotation)),nl,                      
	save_sequence_list_to_file(OutputFile,OutputAnnotation), % Save result to filename given
	*/
	open(OutputFile,write,OutStream),
	write('prediction('), write(OutputAnnotation), writeln(').'),
	close(OutStream),
	write('LoSt consorf genefinder terminated successfully.'),nl.                                         