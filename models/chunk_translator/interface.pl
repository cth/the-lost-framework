:- ['../../lost.pl'].
:- lost_include_api(interface).
:- lost_include_api(misc_utils).
:- lost_include_api(io).
:- lost_include_api(genecode).

lost_input_formats(lost_best_annotation, [text(prolog(ranges(gene)))]).
lost_output_format(lost_best_annotation, _Options, text(fasta(_))).

lost_option(lost_best_annotation,mode,0,'Mode of translation of the chunk'). 
lost_option(lost_best_annotation,genecode,11,'Genecode table'). 

%	Required options ase:
%			mode(translation_mode)
%		where translation_mode = 0 means entire chunk is translated, 
%		and translation_mode = 1 means only longest orf is translated
lost_option_values(lost_best_annotation,mode,[0,1]).



% This is what is used to get the best annotation
% requires translationmode{0,1} and gencodefile.
lost_best_annotation([Chunk_File],Options,Translated_Chunk_File) :-
	write('LoSt chunk Translator: '),nl,
				get_option(Options,mode,Mode),
                                get_option(Options,genecode,Genecode_Number),
				cl('chunk_translator.pl'), % Load the actual PRISM model
				chunk_translator(Chunk_File,Mode,Genecode_Number,Translated_Chunk_File),
				write('LoSt chunk translator completed succesfully'),nl.
