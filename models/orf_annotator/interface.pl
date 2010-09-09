:- ['../../lost.pl'].
:- lost_include_api(interface).
:- lost_include_api(misc_utils).
:- lost_include_api(io).

lost_input_formats(annotate, [text(prolog(ranges(gene)))]).
lost_output_format(annotate, _Options, text(prolog(ranges(gene)))).


annotate([Chunk_File],_Options,Orf_Annot_File) :-
	write('LoSt orf annotator: '),nl,
        consult('orf_annotator.pl'), % Load the actual PRISM model
        orf_annotation(Chunk_File,Orf_Annot_File),
        writeln('Orf annotation succeeds !!'),
	write('wrote file :'), writeln(Orf_Annot_File).


