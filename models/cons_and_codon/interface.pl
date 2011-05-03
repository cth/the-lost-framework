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
% Joint model   %
%%%%%%%%%%%%%%%


annotate_joint_model([ParamsFile,InputFile],_Options,OutputFile) :-
	write('joint model genefinder: '),nl,
   	prismAnnot('joint_model'), % Load the actual PRISM model
        restore_sw(ParamsFile),
	open(OutputFile,write,StreamOut),
	open(InputFile,read,StreamIn),
	compute_and_save_annotations(StreamIn,StreamOut,1),
	close(StreamOut),
	close(StreamIn).


parallel_annotate_joint_model([ParamsFile,InputFile],_,OutputFile) :-
        split_file(InputFile,500,'input', '.pl', ResultingFiles),
        open('input_files.list',write,OutS),
        forall(member(File,ResultingFiles),(write(OutS,File),write(OutS,'\n'))),
        close(OutS),
        atom_concat_list(['./parallel_predict.sh ','10 ', 'annotate_joint_model ', ParamsFile, ' ', OutputFile, ' input_files.list'], Cmd),
        system(Cmd).


learn_joint_model([ChunkFileWithRef],_,ParamsFile) :-
	TrainingFile = 'training_data.pl',
	make_training_file_joint_model(ChunkFileWithRef,TrainingFile),
        prism(joint_model),
        ['joint_model.psm'],
	viterbi_learn_file(TrainingFile),
	save_sw(ParamsFile).

parallel_learn_joint_model([ChunkFileWithRef],_,ParamsFile) :-
        lost_tmp_directory(Tmp),
        atom_concat_list([Tmp,'joint_model_full_train.pl'],TrainingFile),
        make_training_file_joint_model(ChunkFileWithRef,TrainingFile),
        write(here),nl,
        split_file(TrainingFile,1000,'input_train_joint_model', '.pl',ResultingFiles),
        open('training_files.list',write,OutS),
        forall(member(File,ResultingFiles), (write(OutS,File), write(OutS,'\n'))),
        close(OutS),
        atom_concat_list(['./parallel_train.sh ','6  ', 'joint_model ',  ParamsFile, ' training_files.list'], Cmd),
        system(Cmd).




make_training_file_joint_model(Chunk_Annot,Training_File):-
	open(Chunk_Annot,read,Chunk_Stream),
	open(Training_File,write,Train_Stream),
	make_training_file_rec_joint_model(Chunk_Stream, Train_Stream),
	close(Train_Stream),
	close(Chunk_Stream).


make_training_file_rec_joint_model(InStream,OutStream) :-
	read(InStream,Chunk),
	((Chunk==end_of_file) ->
		true 
		;
		Chunk =.. [ _Functor, _SeqId, _Left, _Right, _Strand, _Frame, Extra],
		member(gb(RefAnnot),Extra),
		member(identity_seq(IdSeq),Extra),
                member(sequence(Nucleotides),Extra),
                merge_nucleotides_and_identity(Nucleotides,IdSeq,Combined),
                Goal =.. [ joint_model, Combined,  RefAnnot ], 
		write(OutStream,Goal), 
		write(OutStream,'.\n'),
		make_training_file_rec_joint_model(InStream,OutStream)).


%%%%%%%%%%%%%%%%%%%%%
% Basic combiner    %
%%%%%%%%%%%%%%%%%%%%%

annotate_combiner([ParamsFile,InputFile],_Options,OutputFile) :-                                 
   	prismAnnot('combiner'), % Load the actual PRISM model
        restore_sw(ParamsFile),
	open(InputFile,read,StreamIn),
	open(OutputFile,write,StreamOut),
	compute_and_save_annotations_combiner(StreamIn,StreamOut,1),
	close(StreamOut),
	close(StreamIn).

parallel_annotate_combiner([ParamsFile,InputFile],_,OutputFile) :-
        split_file(InputFile,500,'blastgf_single_input', '.pl', ResultingFiles),
        open('input_files.list',write,OutS),
        forall(member(File,ResultingFiles),(write(OutS,File),write(OutS,'\n'))),
        close(OutS),
        atom_concat_list(['./parallel_predict.sh ','8 ', 'annotate_combiner ', ParamsFile, ' ', OutputFile, ' input_files.list'], Cmd),
        system(Cmd).

parallel_learn_combiner([ChunkFileWithRef],_,ParamsFile) :-
        lost_tmp_directory(Tmp),
        atom_concat_list([Tmp,'combiner_full_train.pl'],TrainingFile),
        make_training_file_combiner(ChunkFileWithRef,TrainingFile),
        split_file(TrainingFile,1000,'input_train_combiner', '.pl',ResultingFiles),
        open('training_files.list',write,OutS),
        forall(member(File,ResultingFiles), (write(OutS,File), write(OutS,'\n'))),
        close(OutS),
        atom_concat_list(['./parallel_train.sh ','6 ', 'combiner ',  ParamsFile, ' training_files.list'], Cmd),
        system(Cmd).


learn_combiner([ChunkFileWithRef],_,ParamsFile) :-
	TrainingFile = 'training_data.pl',
	make_training_file_combiner(ChunkFileWithRef,TrainingFile),
        prism(combiner),
        ['combiner.psm'],
	viterbi_learn_file(TrainingFile),
	save_sw(ParamsFile).


compute_and_save_annotations_combiner(StreamIn,Stream_Out,Nb_Iterations) :-
		read(StreamIn,Chunk),
		((Chunk == end_of_file) ->
			true,
			write('compute_and_save_annotations done.'),nl
			;
			Chunk =.. [ _F,SeqId, Left, Right, Strand, Frame, Extra],
                        member(blastgf(Prediction),Extra),
                        member(sequence(Nucleotides),Extra),
                merge_nucleotides_and_prediction(Nucleotides,Prediction,Combined),

        	check_or_fail(viterbit(combiner_noannot(Combined),_P,Tree),error('Viterbi computation failed')),
                flatten(Tree,FlatTree),
                findall(State,member(msw(emit(State),_),FlatTree),States),
                reverse(States,RevStates),
                build_state_annotation(RevStates,Annotation),
        	build_term_for_annotation(combiner,SeqId,[Left,Right],Strand,Frame,Annotation,Term), (var(Term) ->
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
        	compute_and_save_annotations_combiner(StreamIn,Stream_Out,Nb_Iterations1)).

make_training_file_combiner(Chunk_Annot,Training_File):-
	open(Chunk_Annot,read,Chunk_Stream),
	open(Training_File,write,Train_Stream),
	make_training_file_combiner_rec(Chunk_Stream, Train_Stream),
	close(Train_Stream),
	close(Chunk_Stream).
	
make_training_file_combiner_rec(InStream,OutStream) :-
	read(InStream,Chunk),
	((Chunk==end_of_file) ->
		true 
		;
		Chunk =.. [ _Functor, _SeqId, _Left, _Right, _Strand, _Frame, Extra],
		member(gb(RefAnnot),Extra),
                member(blastgf(Prediction),Extra),
                member(sequence(Nucleotides),Extra),
                merge_nucleotides_and_prediction(Nucleotides,Prediction,Combined),
%                write(merge_nucleotides_and_prediction(Nucleotides,Prediction,Combined)),
                Goal =.. [ combiner, Combined,  RefAnnot ], 
		write(OutStream,Goal), 
		write(OutStream,'.\n'),
		make_training_file_combiner_rec(InStream,OutStream)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Combiner: codpref + length + blastgf
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

annotate_combiner_length([ParamsFile,InputFile],_Options,OutputFile) :-
   	prismAnnot('combiner_length'), % Load the actual PRISM model
        restore_sw(ParamsFile),
	open(InputFile,read,StreamIn),
	open(OutputFile,write,StreamOut),
	compute_and_save_annotations_combiner_length(StreamIn,StreamOut,1),
	close(StreamOut),
	close(StreamIn).


parallel_annotate_combiner_length([ParamsFile,InputFile],_,OutputFile) :-
        split_file(InputFile,500,'blastgf_single_input', '.pl', ResultingFiles),
        open('input_files.list',write,OutS),
        forall(member(File,ResultingFiles),(write(OutS,File),write(OutS,'\n'))),
        close(OutS),
        atom_concat_list(['./parallel_predict.sh ','10 ', 'annotate_combiner_length ', ParamsFile, ' ', OutputFile, ' input_files.list'], Cmd),
        system(Cmd).

parallel_annotate_combiner_length([ParamsFile,InputFile],_,OutputFile) :-
        split_file(InputFile,500,'blastgf_single_input', '.pl', ResultingFiles),
        open('input_files.list',write,OutS),
        forall(member(File,ResultingFiles),(write(OutS,File),write(OutS,'\n'))),
        close(OutS),
        atom_concat_list(['./parallel_predict.sh ','10 ', 'annotate_combiner_length ', ParamsFile, ' ', OutputFile, ' input_files.list'], Cmd),
        system(Cmd).



compute_and_save_annotations_combiner_length(StreamIn,Stream_Out,Nb_Iterations) :-
		read(StreamIn,Chunk),
		((Chunk == end_of_file) ->
			true,
			write('compute_and_save_annotations_combiner_length done.'),nl
			;
			Chunk =.. [ _F,SeqId, Left, Right, Strand, Frame, Extra],
                        member(blastgf(Prediction),Extra),
                        member(sequence(Nucleotides),Extra),
                        member(length_range_annot(LengthRangeAnnot),Extra),
                merge_nucleotides_and_prediction(Nucleotides,Prediction,Combined1),
                write('length range annot: '), write(LengthRangeAnnot),nl,
                merge_length_annot(Combined1,LengthRangeAnnot,Combined),

                write(viterbit(combiner_noannot(Combined),P,Tree)),nl,

                % We use soft check instead, since some ORFs may not
                % have stops and this will fail for those
        	%check_or_fail(viterbit(combiner_noannot(Combined),_P,Tree),error('Viterbi computation faile (combiner length')),
        	(viterbit(combiner_noannot(Combined),_P,Tree) ->
                        true
                        ;
                        write('Viterbi computation failed for goal: '),
                        write(combiner_noannot(Combined))),
                flatten(Tree,FlatTree),
                findall(State,member(msw(emit(State),_),FlatTree),States),
                reverse(States,RevStates),
                write(RevStates),nl,
                build_state_annotation(RevStates,Annotation),
                write(Annotation),nl,
        	write(build_term_for_annotation(combiner_length,SeqId,[Left,Right],Strand,Frame,Annotation,Term)),nl, 
        	build_term_for_annotation(combiner_length,SeqId,[Left,Right],Strand,Frame,Annotation,Term), 
                write('term is :'),
                write(Term),nl,
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
        	compute_and_save_annotations_combiner_length(StreamIn,Stream_Out,Nb_Iterations1)).




parallel_learn_combiner_length([ChunkFileWithRef],_,ParamsFile) :-
        lost_tmp_directory(Tmp),
        atom_concat_list([Tmp,'combiner_full_train.pl'],TrainingFile),
        make_training_file_combiner_length(ChunkFileWithRef,TrainingFile),
        split_file(TrainingFile,1000,'input_train_combiner_length', '.pl',ResultingFiles),
        open('training_files.list',write,OutS),
        forall(member(File,ResultingFiles), (write(OutS,File), write(OutS,'\n'))),
        close(OutS),
        atom_concat_list(['./parallel_train.sh ','10 ', 'combiner_length ',  ParamsFile, ' training_files.list'], Cmd),
        system(Cmd).


learn_combiner_length([ChunkFileWithRef],_,ParamsFile) :-
	TrainingFile = 'training_data.pl',
	make_training_file_combiner(ChunkFileWithRef,TrainingFile),
        prism(combiner_length),
        ['combiner_length.psm'],
	viterbi_learn_file(TrainingFile),
	save_sw(ParamsFile).


make_training_file_combiner_length(Chunk_Annot,Training_File):-
	open(Chunk_Annot,read,Chunk_Stream),
	open(Training_File,write,Train_Stream),
	make_training_file_combiner_length_rec(Chunk_Stream, Train_Stream),
	close(Train_Stream),
	close(Chunk_Stream).
	
make_training_file_combiner_length_rec(InStream,OutStream) :-
	read(InStream,Chunk),
	((Chunk==end_of_file) ->
		true 
		;
		Chunk =.. [ _Functor, _SeqId, _Left, _Right, _Strand, _Frame, Extra],
		member(gb(RefAnnot),Extra),
                member(blastgf(Prediction),Extra),
                member(sequence(Nucleotides),Extra),
                member(length_range_annot(LengthRangeAnnot), Extra),
                merge_nucleotides_and_prediction(Nucleotides,Prediction,Combined1),
                merge_length_annot(Combined1,LengthRangeAnnot,Combined),
                %write(merge_nucleotides_and_prediction(Nucleotides,Prediction,Combined)),
                Goal =.. [ combiner, Combined,  RefAnnot ], 
		write(OutStream,Goal), 
		write(OutStream,'.\n'),
		make_training_file_combiner_length_rec(InStream,OutStream)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Combiner: codpref and length 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

annotate_codpref_w_length([ParamsFile,InputFile],_Options,OutputFile) :-
   	prismAnnot('codpref_w_length'), % Load the actual PRISM model
        restore_sw(ParamsFile),
	open(InputFile,read,StreamIn),
	open(OutputFile,write,StreamOut),
	compute_and_save_annotations_combiner_length(StreamIn,StreamOut,1),
	close(StreamOut),
	close(StreamIn).


parallel_annotate_codpref_w_length([ParamsFile,InputFile],_,OutputFile) :-
        split_file(InputFile,500,'cTripletsodpref_w_length_input', '.pl', ResultingFiles),
        open('input_files.list',write,OutS),
        forall(member(File,ResultingFiles),(write(OutS,File),write(OutS,'\n'))),
        close(OutS),
        atom_concat_list(['./parallel_predict.sh ','10 ', 'codpref_w_length', ParamsFile, ' ', OutputFile, ' input_files.list'], Cmd),
        system(Cmd).

parallel_annotate_codpref_w_length([ParamsFile,InputFile],_,OutputFile) :-
        split_file(InputFile,500,'codpref_w_length_input', '.pl', ResultingFiles),
        open('input_files.list',write,OutS),
        forall(member(File,ResultingFiles),(write(OutS,File),write(OutS,'\n'))),
        close(OutS),
        atom_concat_list(['./parallel_predict.sh ','10 ', 'codpref_w_length', ParamsFile, ' ', OutputFile, ' input_files.list'], Cmd),
        system(Cmd).


compute_and_save_annotations_codpref_w_length(StreamIn,Stream_Out,Nb_Iterations) :-
		read(StreamIn,Chunk),
		((Chunk == end_of_file) ->
			true,
			write('compute_and_save_annotations_codpref_w_length done.'),nl
			;
			Chunk =.. [ _F,SeqId, Left, Right, Strand, Frame, Extra],
                        member(sequence(Nucleotides),Extra),
                        member(length_range_annot(LengthRangeAnnot),Extra),
                nucleotide_triplets(Nucleotides,Triplets),
                write('length range annot: '), write(LengthRangeAnnot),nl,
                merge_length_annot(Triplets,LengthRangeAnnot,Combined),

                write(viterbit(combiner_noannot(Combined),P,Tree)),nl,

                % We use soft check instead, since some ORFs may not
                % have stops and this will fail for those
        	%check_or_fail(viterbit(combiner_noannot(Combined),_P,Tree),error('Viterbi computation faile (combiner length')),
        	(viterbit(combiner_noannot(Combined),_P,Tree) ->
                        true
                        ;
                        write('Viterbi computation failed for goal: '),
                        write(combiner_noannot(Combined))),
                flatten(Tree,FlatTree),
                findall(State,member(msw(emit(State),_),FlatTree),States),
                reverse(States,RevStates),
                write(RevStates),nl,
                build_state_annotation(RevStates,Annotation),
                write(Annotation),nl,
        	write(build_term_for_annotation(codpref_w_length,SeqId,[Left,Right],Strand,Frame,Annotation,Term)),nl, 
        	build_term_for_annotation(codpref_w_length,SeqId,[Left,Right],Strand,Frame,Annotation,Term), 
                write('term is :'),
                write(Term),nl,
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
        	compute_and_save_annotations_codpref_w_length(StreamIn,Stream_Out,Nb_Iterations1)).




parallel_learn_codpref_w_length([ChunkFileWithRef],_,ParamsFile) :-
        lost_tmp_directory(Tmp),
        atom_concat_list([Tmp,'codpref_w_length_full_train.pl'],TrainingFile),
        make_training_file_combiner_length(ChunkFileWithRef,TrainingFile),
        split_file(TrainingFile,1000,'input_codpref_w_length', '.pl',ResultingFiles),
        open('training_files.list',write,OutS),
        forall(member(File,ResultingFiles), (write(OutS,File), write(OutS,'\n'))),
        close(OutS),
        atom_concat_list(['./parallel_train.sh ','10 ', 'codpref_w_length ',  ParamsFile, ' training_files.list'], Cmd),
        system(Cmd).


learn_combiner_codpref_w_length([ChunkFileWithRef],_,ParamsFile) :-
	TrainingFile = 'training_data.pl',
	make_training_file_combiner(ChunkFileWithRef,TrainingFile),
        prism(combiner_length),
        ['codpref_w_length.psm'],
	viterbi_learn_file(TrainingFile),
	save_sw(ParamsFile).


make_training_file_codpref_w_length(Chunk_Annot,Training_File):-
	open(Chunk_Annot,read,Chunk_Stream),
	open(Training_File,write,Train_Stream),
	make_training_file_codpref_w_length_rec(Chunk_Stream, Train_Stream),
	close(Train_Stream),
	close(Chunk_Stream).
	
make_training_file_codpref_w_length_rec(InStream,OutStream) :-
	read(InStream,Chunk),
	((Chunk==end_of_file) ->
		true 
		;
		Chunk =.. [ _Functor, _SeqId, _Left, _Right, _Strand, _Frame, Extra],
		member(gb(RefAnnot),Extra),
                member(sequence(Nucleotides),Extra),
                member(length_range_annot(LengthRangeAnnot), Extra),
                nucleotide_triplets(Nucleotides,Triplets),
                merge_length_annot(Triplets,LengthRangeAnnot,Combined),
                Goal =.. [ combiner, Combined,  RefAnnot ], 
		write(OutStream,Goal), 
		write(OutStream,'.\n'),
		make_training_file_codpref_w_length_rec(InStream,OutStream)).




%%%%%%%%%%%% Utility predicates %%%%%%%%%%%%%%

build_state_annotation([],[]).
build_state_annotation([n|StatesRest],[0,0,0|AnnotRest]) :-
        build_state_annotation(StatesRest,AnnotRest).
build_state_annotation([start|StatesRest],[1,1,1|AnnotRest]) :-
        build_state_annotation(StatesRest,AnnotRest).
build_state_annotation([stop|StatesRest],[1,1,1|AnnotRest]) :-
        build_state_annotation(StatesRest,AnnotRest).
build_state_annotation([c|StatesRest],[1,1,1|AnnotRest]) :-
        build_state_annotation(StatesRest,AnnotRest).


% build_term_for_annotation :
% Terms is a variable if the annotation is a list of 0, otherwise a term with a range for the coding region
build_term_for_annotation(Functor,SeqId,[Left,Right],Dir,Frame,Annotation,Term) :-
        first_coding(Left,1,Annotation,Start),
        !,
        Term =..[Functor,SeqId,Left,Right,Dir,Frame,[annotation(Annotation),start(Start)]].

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

nucleotide_triplets([],[]).
nucleotide_triplets([N1,N2,N3|Ns],[[N1,N2,N3]|Rest]) :-
        nucleotide_triplets(Ns,Rest).

merge_nucleotides_and_prediction([],[],[]).
merge_nucleotides_and_prediction([N1,N2,N3|Ns],[P1,P2,P3|Ps],[[[N1,N2,N3],[P1,P2,P3]]|Rest]) :-
       merge_nucleotides_and_prediction(Ns,Ps,Rest). 

merge_nucleotides_and_identity([],[],[]).
merge_nucleotides_and_identity([N1,N2,N3|Ns],[I|Is],[[[N1,N2,N3],I]|Rest]) :-
       merge_nucleotides_and_identity(Ns,Is,Rest). 

merge_length_annot([],[],[]).
merge_length_annot([A|As],[L|Ls],[Merged|Rest]) :-
        append(A,[L],Merged),
        merge_length_annot(As,Ls,Rest).
