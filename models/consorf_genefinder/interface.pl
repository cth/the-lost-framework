:- ['../../lost.pl'].                                                                   
:- lost_include_api(interface).                                                         
:- lost_include_api(autoAnnotations).
:- lost_include_api(misc_utils).
:- lost_include_api(io).                                                                                                                                        
                                                                                        
% This is what is used to get the best annotation                                       
lost_best_annotation([OrfFile,ConsFile],Options,OutputFile) :-                                 
	write('LoSt consorf genefinder: '),nl,                                                         
	write(lost_best_annotation(ParamFile,[OrfFile,ConsFile],Options,OutputFile)),nl,             
	
				lost_required_option(Options,parameter_file,ParamFile),                               
				prismAnnot('consorf_genefinder'), % Load the actual PRISM model                                         
	
	write(restore_sw(ParamFile)),nl, % Restore switch values                              
	
	restore_sw(ParamFile), % Restore switch values 
	write('parameters loaded'),nl,                           
		     
		    load_annotation_from_file(sequence,[data_position(1),range(1,48)],OrfFile,ID),
	write('id '),writeln(ID), 
		    load_annotation_from_file(sequence,[data_position(2),range(1,48)],OrfFile,Start),
	write('start '),writeln(Start),
		    load_annotation_from_file(sequence,[data_position(3),range(1,48)],OrfFile,Stop),
	write('stop '),writeln(Stop),		    
		    load_annotation_from_file(sequence,[data_position(4),range(1,48)],OrfFile,Dir),
	write('dir '),writeln(Dir),
		    load_annotation_from_file(sequence,[data_position(5),range(1,48)],OrfFile,Frm),  
  write('frm '),writeln(Frm),
  			load_annotation_from_file(sequence,[data_position(6),range(1,48)],OrfFile,InputOrf),
   			
   			load_annotation_from_file(sequence,[data_position(6),range(1,48)],ConsFile,InputCons),      
  
        % Derive an annotation somehow
                                                          
  write(viterbiAnnot(consorf(InputOrf,InputCons,OutputAnnotation1))),nl,                      
	
				check_or_fail(viterbiAnnot(consorf(InputOrf,InputCons,OutputAnnotation),_),                        
                        error(viterbiAnnot_says_no_no_no)),
        OutputEntry =.. [consorf_prediction,Id,Start,Stop,Dir,Frm,OutputAnnotation],                                 
  
  write('LoSt consorf genefinder produced output prediction: '),                                    
  write(OutputEntry),nl,                                                       
				
	/*			
	write(save_sequence_list_to_file(OutputFile,OutputAnnotation)),nl,                      
	save_sequence_list_to_file(OutputFile,OutputAnnotation), % Save result to filename given
	*/
	
				open(OutputFile,write,OutStream),
				writeq(OutStream,OutputEntry),writeln(OutStream,'.'),
				close(OutStream),
	
	
	write('LoSt consorf genefinder terminated successfully.'),nl.                                         