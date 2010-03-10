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
			  writeln('foobar 1'),
			  open(OrfFile, read, OrfIn,[alias(orfin)]),
			  open(ConsFile, read, ConsIn,[alias(consin)]),
			  writeln('foobar 2'),
			  % writeq('read(orfin,OrfTerm)'),nl,
			  read(orfin,OrfTerm),
			  read(consin,ConsTerm),
			  writeln('foobar 3'),
			  OrfTerm =.. [_,Id,Start1,Stop1,Dir,Frm,InputOrf1|_],
			  ConsTerm =.. [_,Id,Start1,Stop1,Dir,Frm,InputCons1|_],
			  writeln('foobar 4'),
			  close(orfin),
			  close(consin),
        % Derive an annotation somehow
                                                          
  	
				check_or_fail(viterbiAnnot(consorf(InputOrf1,InputCons1,OutputAnnotation1),_),                        
                        error(viterbiAnnot_says_no_no_no)),
        /*
        check_or_fail(viterbiAnnot(consorf(InputOrf2,InputCons2,OutputAnnotation2),_),                        
                        error(viterbiAnnot_says_no_no_no)),
 				*/
        OutputEntry1 =.. [consorf_prediction,Id,Start1,Stop1,Dir,Frm,OutputAnnotation1],
        % OutputEntry2 =.. [consorf_prediction,Id,Start2,Stop2,Dir,Frm,OutputAnnotation2],                                 
  
  write('LoSt consorf genefinder produced output prediction: '),                                    
  
				
	/*			
	write(save_sequence_list_to_file(OutputFile,OutputAnnotation)),nl,                      
	save_sequence_list_to_file(OutputFile,OutputAnnotation), % Save result to filename given
	*/
	
				open(OutputFile,write,OutStream),
				writeq(OutStream,OutputEntry1),writeln(OutStream,'.'),
				% writeq(OutStream,OutputEntry2),writeln(OutStream,'.'),
				close(OutStream),
	
	
	write('LoSt consorf genefinder terminated successfully.'),nl.                                         