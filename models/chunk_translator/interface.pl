:- ['../../lost.pl'].
:- lost_include_api(interface).
:- lost_include_api(misc_utils).
:- lost_include_api(io).
:- lost_include_api(genecode).

:- task(translate([text(prolog(ranges(gene)))], [mode(0),genecode(11)], text(fasta(ffa)))).

lost_input_formats(annotate, [text(prolog(ranges(gene)))]).
lost_output_format(annotate, _Options, text(fasta(ffa))).

lost_option(annotate,mode,0,'Mode of translation of the chunk'). 
lost_option(annotate,genecode,11,'Genecode table'). 

%	Required options ase:
%			mode(translation_mode)
%		where translation_mode = 0 means entire chunk is translated, 
%		and translation_mode = 1 means only longest orf is translated
lost_option_values(annotate,mode,[0,1]).

%% translate(+InputFiles, +Options, +OutputFile)
% == 
%  InputFiles = [ ChunkFile ]
% ==
% Translates each chunk in ChunkFile to get the amino acid sequence. 
% The option =|mode|= determines whether to run translate entire chunk =|mode=0|= or the longest orf in chunk =|mode=1|=.
% The translated chunks are written OutputFile which in a multi-fasta format.
annotate([Chunk_File],Options,Translated_Chunk_File) :-
	write('LoSt chunk Translator: '),nl,
	get_option(Options,mode,Mode),
	get_option(Options,genecode,Genecode_Number),
	cl('chunk_translator.pl'), % Load the actual PRISM model
	chunk_translator(Chunk_File,Mode,Genecode_Number,Translated_Chunk_File),
	write('LoSt chunk translator completed succesfully'),nl.
