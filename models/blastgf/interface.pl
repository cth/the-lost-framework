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

annotate_single_track([ParamsFile,InputFile],_Options,OutputFile) :-
	write('BLAST genefinder: '),nl,
   	prismAnnot('blastgf_single_track'), % Load the actual PRISM model
        restore_sw(ParamsFile),
	open(OutputFile,write,StreamOut),
	open(InputFile,read,StreamIn),
	compute_and_save_annotations(StreamIn,StreamOut,1),
	close(StreamOut),
	close(StreamIn).

annotate_multi_track([ParamsFile,InputFile],_Options,OutputFile) :-                                 
	write('BLAST genefinder: '),nl,
   	prismAnnot('blastgf_multi_track'), % Load the actual PRISM model
        restore_sw(ParamsFile),
	open(InputFile,read,StreamIn),
	open(OutputFile,write,StreamOut),
	compute_and_save_annotations(StreamIn,StreamOut,1),
	close(StreamOut),
	close(StreamIn).

parallel_annotate_multi_track([ParamsFile,InputFile],_,OutputFile) :-
        split_file(InputFile,500,'blastgf_multi_input', '.pl', ResultingFiles),
        open('input_files.list',write,OutS),
        forall(member(File,ResultingFiles),(write(OutS,File),write(OutS,'\n'))),
        close(OutS),
        atom_concat_list(['./parallel_predict.sh ','10 ', 'annotate_multi_track ', ParamsFile, ' ', OutputFile, ' input_files.list'], Cmd),
        system(Cmd).

parallel_annotate_single_track([ParamsFile,InputFile],_,OutputFile) :-
        split_file(InputFile,1000,'blastgf_single_input', '.pl', ResultingFiles),
        open('input_files.list',write,OutS),
        forall(member(File,ResultingFiles),(write(OutS,File),write(OutS,'\n'))),
        close(OutS),
        atom_concat_list(['./parallel_predict.sh ','10 ', 'annotate_single_track ', ParamsFile, ' ', OutputFile, ' input_files.list'], Cmd),
        system(Cmd).


build_state_annotation([],[]).
build_state_annotation([n|StatesRest],[0,0,0|AnnotRest]) :-
        build_state_annotation(StatesRest,AnnotRest).
build_state_annotation([c|StatesRest],[1,1,1|AnnotRest]) :-
        build_state_annotation(StatesRest,AnnotRest).


compute_and_save_annotations(StreamIn,Stream_Out,Nb_Iterations) :-
		read(StreamIn,Chunk),
		((Chunk == end_of_file) ->
			true,
			write('compute_and_save_annotations done.'),nl
			;
			Chunk =.. [ _F,SeqId, Left, Right, Strand, Frame, Extra],
			member(identity_seq(IdSeq), Extra),
        	%write(viterbit(blastgf_noannot(IdSeq),_P,Tree)),nl,
        	check_or_fail(viterbit(blastgf_noannot(IdSeq),_P,Tree),error('Viterbi computation failed')),
                flatten(Tree,FlatTree),
                findall(State,member(msw(emit(State),_),FlatTree),States),
                reverse(States,RevStates),
                build_state_annotation(RevStates,Annotation),
        	build_term_for_annotation(blastgf,SeqId,[Left,Right],Strand,Frame,Annotation,Term),
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
        	compute_and_save_annotations(StreamIn,Stream_Out,Nb_Iterations1)).

% build_term_for_annotation :
% Terms is a variable if the annotation is a list of 0, otherwise a term with a range for the coding region
build_term_for_annotation(Functor,SeqId,[Left,Right],Dir,Frame,Annotation,Term) :-
        first_coding(Left,1,Annotation,Start),
        !,
        Term =..[Functor,SeqId,Left,Right,Dir,Frame,[blastgf(Annotation),start(Start)]].

build_term_for_annotation(_Functor,_SeqId,_Range,_Dir,_Frame,_Annotation,_Term).

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

learn_single_track([ChunkFileWithRef],_,ParamsFile) :-
	TrainingFile = 'training_data.pl',
	make_training_file(ChunkFileWithRef,TrainingFile),
        write(here),nl,
	save_sw(ParamsFile).

parallel_learn_multi_track([ChunkFileWithRef],_,ParamsFile) :-
        lost_tmp_directory(Tmp),
        atom_concat_list([Tmp,'blastgf_full_train.pl'],TrainingFile),
        make_training_file(ChunkFileWithRef,TrainingFile),
        split_file(TrainingFile,1000,'blastgf_multi', '.pl',ResultingFiles),
        open('training_files.list',write,OutS),
        forall(member(File,ResultingFiles), (write(OutS,File), write(OutS,'\n'))),
        close(OutS),
        atom_concat_list(['./parallel_train.sh ','10 ', 'blastgf_multi_track ',  ParamsFile, ' training_files.list'], Cmd),
        system(Cmd).

learn_multi_track([ChunkFileWithRef],_,ParamsFile) :-
	TrainingFile = 'training_data.pl',
	make_training_file(ChunkFileWithRef,TrainingFile),
	prismAnnot('blastgf_multi_track',direct),
        prism(blastgf_multi_trackEX),
        ['blastgf_multi_trackEX.psm'],
	viterbi_learn_file(TrainingFile),
	save_sw(ParamsFile).

parallel_learn_single_track([ChunkFileWithRef],_,ParamsFile) :-
        lost_tmp_directory(Tmp),
        atom_concat_list([Tmp,'blastgf_full_train.pl'],TrainingFile),
        make_training_file(ChunkFileWithRef,TrainingFile),
        split_file(TrainingFile,1000,'train_blastgf_single_track', '.pl',ResultingFiles),
        open('training_files.list',write,OutS),
        forall(member(File,ResultingFiles), (write(OutS,File), write(OutS,'\n'))),
        close(OutS),
        atom_concat_list(['./parallel_train.sh ','10 ', 'blastgf_single_track ',  ParamsFile, ' training_files.list'], Cmd),
        system(Cmd).


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
                Goal =.. [ blastgf, IdSeq, RefAnnot ], 
		write(OutStream,Goal), 
		write(OutStream,'.\n'),
		make_training_file_rec(InStream,OutStream)).

