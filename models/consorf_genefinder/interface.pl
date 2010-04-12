:- ['../../lost.pl'].                                                                   
:- lost_include_api(interface).                                                         
:- lost_include_api(autoAnnotations).
:- lost_include_api(misc_utils).
:- lost_include_api(io).                                                                                                                                        

lost_input_formats(lost_best_annotation, [text(prolog(ranges(gene))),text(prolog(ranges(gene)))]).
lost_output_format(lost_best_annotation, _Options, [text(prolog(ranges(gene)))]).
                                                                                        
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
			  consorf_main(OrfIn,ConsIn,OutStream),
			  close(orfin),
			  close(consin),
        close(OutStream),
				write('LoSt consorf genefinder terminated successfully.'),nl.
				
/**************************************************************************************/
% Recusive call of the prediction routine
%
consorf_main(OrfInStream,ConsInStream,OutStream):-		  
		% writeq('read(orfin,OrfTerm)'),nl,
		read(OrfInStream,OrfTerm),
		read(ConsInStream,ConsTerm),
		(
		OrfTerm \= eof, ConsTerm \= eof ->			  
			OrfTerm =.. [_,Id,Start,Stop,Dir,Frm,InputOrf|_],
			ConsTerm =.. [_,Id,Start,Stop,Dir,Frm,InputCons|_],
        		% Derive an annotation somehow
        		check_or_fail(viterbiAnnot(consorf(InputOrf,InputCons,OutputAnnotation),_),                        
                     	error(errorpair(InputOrf,InputCons))),
         		OutputEntry =.. [consorf_prediction,Id,Start,Stop,Dir,Frm,OutputAnnotation],
        		writeq(OutStream,OutputEntry),writeln(predout,'.'),
			consorf_main(OrfInStream,ConsInStream,OutStream)
		;
			true
		).				
	
	
	