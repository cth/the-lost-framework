:- ['../../lost.pl'].                                                      
:- lost_include_api(interface).                                                         
:- lost_include_api(autoAnnotations).
:- lost_include_api(misc_utils).
:- lost_include_api(io).                                                                                                                                        
:- lost_include_api(stats).
:- lost_include_api(viterbi_learn).

% Option declaration

% Input Format Specification
lost_input_formats(annotate,[prolog(sequence(_))]).
% Output Format Specification
lost_output_format(annotate,_,text(prolog(ranges(_)))).

%%%%%%%%%%%%%%%
% Prediction  %
%%%%%%%%%%%%%%%

annotate_single_track([InputFile],_Options,OutputFile) :-                                 
	write('BLAST genefinder: '),nl,
   	prismAnnot('blastgf_single_track'), % Load the actual PRISM model
	open(OutputFile,write,StreamOut),
	open(InputFile,read,StreamIn),
	compute_and_save_annotations(StreamIn,StreamOut,1),
	close(StreamOut),
	close(StreamIn).

annotate_multi_track([InputFile],_Options,OutputFile) :-                                 
	write('BLAST genefinder: '),nl,
   	prismAnnot('blastgf_muli_track'), % Load the actual PRISM model
	open(OutputFile,write,StreamOut),
	open(InputFile,read,StreamIn),
	compute_and_save_annotations(StreamIn,StreamOut,1),
	close(StreamOut),
	close(StreamIn).

compute_and_save_annotations(StreamIn,Stream_Out,Nb_Iterations) :-
		read(Chunk,StreamIn),
		((Chunk == end_of_file) ->
			true,
			write('compute_and_save_annotations done.'),nl
			;
			Chunk =.. [ _F, Left, Right, Strand, Frame, Extra],
			member(identity_seq(IdSeq), Extra),
        	check_or_fail(viterbiAnnot(blastgf_annot(IdSeq,Annotation),_P),error('Viterbi computation failed')),
        	build_term_for_annotation(blastgf,[Left,Right],Strand,Frame,Annotation,Term),
       		(var(Term) ->
            	true
        		;
            	write(Stream_Out,Term),write(Stream_Out,'.'),nl(Stream_Out)
        	),
        	Number is Nb_Iterations mod 500,
        	(Number == 1 -> write(Nb_Iterations) ; write('.')),
        	(Number == 0 ->
            	table_remove(blastgf_annot(_,_)),
            	table_remove(blastgf_rec_annot(_,_,_))
        		;
            	true
        	),
			Nb_Iterations1 is Nb_Iterations+1,
        	!,
        	compute_and_save_annotations(Stream_Out,Nb_Iterations1)).

% build_term_for_annotation :
% Terms is a variable if the annotation is a list of 0, otherwise a term with a range for the coding region
build_term_for_annotation(Functor,[Left,Right],Dir,Frame,Annotation,Term) :-
        first_coding(Left,1,Annotation,Start),
        !,
        Term =..[Functor,Left,Right,Dir,Frame,[blastgf(Annotation),start(Start)]].

build_term_for_annotation(_Functor,_Range,_Dir,_Frame,_Annotation,_Term).

% first_coding(++Left,++Value,++Annotation,--Start)
first_coding(_Left,_Value,[],_Start) :-
        fail.

first_coding(Start,Value,[Value|_Rest_Annotations],Start) :-
        !.

first_coding(Left,Value,[_Annotation|Rest_Annotations],Start) :-
        !,
        Left1 is Left+1,
        first_coding(Left1,Value,Rest_Annotations,Start).


%%%%%%%%%%%%%%%
%%%% learning %
%%%%%%%%%%%%%%%

learn([ChunkFileWithRef],_,ParamsFile) :-
	TrainingFile = 'training_data.pl',
	make_training_file(ChunkFileWithRef,TrainingFile),
	prismAnnot('codon_pref'), % Load the actual PRISM model
	viterbi_learn_file(TrainingFile),
	save_sw(ParamsFile).

make_training_file(Chunk_Annot,Training_File):-
	open(Chunk_Annot,read,Chunk_Stream),
	open(Training_File,write,Train_Stream),
	make_training_file_rec(Chunk_Stream, Train_Stream),
	close(Train_Stream),
	close(Chunk_Stream).
	
make_training_file_rec(InStream,OutStream) :-
	read(InStream,Chunk),
	((Chunk==end_of_file) ->
		true 
		;
		Chunk =.. [ _Functor, _SeqId, _Left, _Right, _Strand, _Frame, Extra],
		member(ref_annot(RefAnnot),Extra),
		member(identity_seq(IdSeq),Extra),
		write(OutStream,blastgf(IdSeq,RefAnnot)),
		write(OutStream,'.\n'),
		make_training_file_rec(InStream,OutStream)).

