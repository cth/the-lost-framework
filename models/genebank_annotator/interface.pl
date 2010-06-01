:- ['../../lost.pl'].
:- lost_include_api(interface).
:- lost_include_api(misc_utils).
:- lost_include_api(io).

lost_input_formats(annotate, [text(prolog(ranges(gene))),text(prolog(ranges(gene)))]).
lost_output_format(annotate, _Options, text(prolog(ranges(gene)))).


annotate([Chunk_File,GeneBank_Filtered],_Options,OutputFile) :-
	write('LoSt Genebank annotator: '),nl,
        consult('genebank_annotator.pl'), % Load the actual PRISM model
        genebank_annotation(Chunk_File,GeneBank_Filtered,OutputFile),
        write('Genebank annotation succeeds !!').
