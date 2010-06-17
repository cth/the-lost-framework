:- ['../../lost.pl'].
:- lost_include_api(interface).
:- lost_include_api(misc_utils).
:- lost_include_api(io).

lost_input_formats(annotate, [text(prolog(sequence(_)))]).
lost_output_format(annotate, _Options, text(prolog(ranges(gene)))).

lost_option(annotate,direction,'+','+ for forward strand and - for reverse strand').
lost_option(annotate,frame,1,'Reading frame: 1,2 or 3').
lost_option(annotate,minimal_length,undefined,'Specified a minimal length for the generated ORF').
lost_option(annotate,maxiimal_length,undefined,'Specified a maximal length for the generated ORF').

lost_option_values(annotate,direction,['+','-']).
lost_option_values(annotate,frame,[1,2,3]).

% This is what is used to get the best annotation
% requires direction (+/-) and frame (1,2,3)
annotate([Sequence_File],Options,Orf_Chunk_File) :-
	write('LoSt orf chopper: '),nl,
        get_option(Options,direction,Dir),
        get_option(Options,frame,Frame),
        get_option(Options,minimal_length,Min_Length),
        get_option(Options,maximal_length,Max_Length),
        cl('orf_chopper.pl'),   % Load the actual PRISM model
        open(Sequence_File,read,InputStream,[alias(seqin)]),
        read(seqin,data(Id,_StartPos,_,_)),
        read(InputStream,_),	% to close the file properly
	close(InputStream),
	
        open(Orf_Chunk_File,write,OutputStream,[alias(chunkout)]),
	get_data_from_file(Sequence_File,[],S),
        (Dir = + ->
            DirFactor = 1
        ;
            DirFactor = -1),
        dna_chop_init(S,1,DirFactor,Frame,Min_Length,Max_Length,_Length,Id,OutputStream),
        close(OutputStream),
        write('LoSt orf-chopper completed succesfully'),nl.


