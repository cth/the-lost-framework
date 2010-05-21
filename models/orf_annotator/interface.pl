:- ['../../lost.pl'].
:- lost_include_api(interface).
:- lost_include_api(misc_utils).
:- lost_include_api(io).

lost_input_formats(lost_best_annotation, [text(prolog(ranges(gene)))]).
lost_output_format(lost_best_annotation, _Options, text(prolog(ranges(gene)))).


lost_best_annotation([Chunk_File],_Options,Orf_Annot_File) :-
	write('LoSt orf annotator: '),nl,
        consult('orf_annotator.pl'), % Load the actual PRISM model
        orf_annotation(Chunk_File,Orf_Annot_File),
        write('Orf annotation succeeds !!').


