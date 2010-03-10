:- ['../../lost.pl'].                                                                   
:- lost_include_api(interface).                                                         
:- lost_include_api(autoAnnotations).
:- lost_include_api(misc_utils).
:- lost_include_api(io).                                                                                                                                        
                                                                                        
% This is what is used to get the best annotation                                       
lost_best_annotation([OrfFile,ConsFile],Options,OutputFile) :-                                 
	write('LoSt consorf genefinder: '),nl,                                                         
				%write(lost_best_annotation(ParamFile,[OrfFile,ConsFile],Options,OutputFile)),nl,             
	
				lost_required_option(Options,parameter_file,ParamFile),                               
				prismAnnot('consorf_genefinder'), % Load the actual PRISM model                                         
				restore_sw(ParamFile), % Restore switch values 
				% write('parameters loaded'),nl,
	
			  open(OrfFile, read, OrfIn,[alias(orfin)]),
			  open(ConsFile, read, ConsIn,[alias(consin)]),
			  open(OutputFile,write,OutStream,[alias(predout)]),
			  prediction_routine,
			  close(orfin),
			  close(consin),
        close(OutStream),
				write('LoSt consorf genefinder terminated successfully.'),nl.
				
prediction_routine:-		  
			  % writeq('read(orfin,OrfTerm)'),nl,
			  read(orfin,OrfTerm),
			  read(consin,ConsTerm),
			  (
			  OrfTerm \= eof, ConsTerm \= eof ->			  
			  	OrfTerm =.. [_,Id,Start,Stop,Dir,Frm,InputOrf|_],
			  	ConsTerm =.. [_,Id,Start,Stop,Dir,Frm,InputCons|_],
			  		  
        	% Derive an annotation somehow
        	check_or_fail(viterbiAnnot(consorf(InputOrf,InputCons,OutputAnnotation),_),                        
                        error(viterbiAnnot_says_no_no_no)),
        
        	OutputEntry =.. [consorf_prediction,Id,Start,Stop,Dir,Frm,OutputAnnotation],
        
  			
					writeq(OutStream,OutputEntry),writeln(OutStream,'.'),
					prediction_routine
				;
					true
				).
				
	
	
	