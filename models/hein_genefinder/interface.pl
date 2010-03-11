:- ['../../lost.pl'].                                                                   
:- lost_include_api(interface).                                                         
:- lost_include_api(autoAnnotations).
:- lost_include_api(misc_utils).
:- lost_include_api(io).                                                                                                                                        
                                                                                        
% 

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
				
				
	
