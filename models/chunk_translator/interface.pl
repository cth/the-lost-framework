:- ['../../lost.pl'].                                                                   
:- lost_include_api(interface).                                                         
:- lost_include_api(misc_utils).
:- lost_include_api(io).                                                                                                                                        
                                                                                        
% This is what is used to get the best annotation 
% requires translationmode{0,1} and gencodefile.                                      
lost_best_annotation([Chunk_File],Options,Translated_Chunk_File) :-                                 
	write('LoSt orf chopper: '),nl,                                                         
				lost_required_option(Options,mode,Mode),
				lost_required_option(Options,genecodefile,Codefile),
				cl(Codefile),
				cl('chunk_translator.pl'), % Load the actual PRISM model                                         
				chunk_translator(Chunk_File,Mode,Translated_Chunk_File),
				write('LoSt chunk translator completed succesfully'),nl.