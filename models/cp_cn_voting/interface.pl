:- ['../../lost.pl'].                                                                   
:- lost_include_api(interface).                                                         
%:- [autoAnnotations].
:- lost_include_api(autoAnnotations). % (bug i denne her ?)
:- lost_include_api(prism_parallel).
:- lost_include_api(misc_utils).
:- lost_include_api(io).                                                                                                                                        
:- lost_include_api(stats).
:- lost_include_api(viterbi_learn).

% Option declaration


% Input Format Specification
lost_input_formats(annotate,[prolog(sequence(_))]).
% Output Format Specification
lost_output_format(annotate,_,text(prolog(ranges(_)))).
set_params:- restore_sw('params_half.gen').
% 
annotate([ParamsFile,InputFile],_Options,OutputFile) :-                                 
	write('Codon preference/conservation voting genefinder: '),nl,                                                         
        prismAnnot('cp_cn_voting'), % Load the actual PRISM model                                         
        restore_sw(ParamsFile),
        % Building of the Input for the annotations
        consult(InputFile),
        findall((Cp,Cn),get_chunk((Cp,Cn)),List_CpCn),	
	 findall([ID,Left,Right,Dir,Frame],chunk(ID,Left,Right,Dir,Frame,_),List_Ranges_Chunk),
	 			% Computation of annotations
        open(OutputFile,write,Stream_Out),
        compute_and_save_annotations(Stream_Out,1,List_CpCn,List_Ranges_Chunk).
        % Save the annotations in the right format
        %write(save_annotation(lost_prediction,List_Ranges_ORF,List_Annotations,Dir,Frame,OutputFile)),nl,
        %save_annotation(lost_prediction,List_Ranges_ORF,Dir,Frame,List_Annotations,OutputFile). % Måske, something more generic to include in io 
        %save_annotation_to_sequence_file(genemark_genefinder,70,Annotation,OutputFile).

parallel_annotate([ParamsFile,InputFile],_opts,OutputFile) :-
        split_file(InputFile,500,'cp_cn_input', '.pl',ResultingFiles),
        open('input_files.list',write,OutS),
        forall(member(File,ResultingFiles), (write(OutS,File), write(OutS,'\n'))),
        close(OutS),
        atom_concat_list(['./parallel_predict.sh ','14 ', ParamsFile, ' ', OutputFile, ' input_files.list'], Cmd),
        system(Cmd).


get_chunk((Cp,Cn)):-
	chunk(_, _, _, D, _,Extra),
	member(codon_pref(CpRev),Extra),
	member(blastgf(CnRev),Extra),
        (D = '-' -> reverse(CpRev,Cp),reverse(CnRev,Cn)
        ;
        Cp = CpRev, Cn = CnRev
        ).
	


%annotate([InputFile],Options,OutputFile) :-
%	get_option(Options,optimized,true),
%	subtract(Options,[optimized(true)],NewOptions1),
%	append([optimized(false)], NewOptions1, NewOptions2),
%	terms_to_file('options.pl',[original_options(NewOptions2)]),
%	lost_tmp_directory(Tmp),
%	atom_concat(Tmp,'lost_genefinder_chunk',Prefix),
%	split_file(InputFile, 10, Prefix, '.pl'),
%	atom_concat(Tmp,'lost_genefinder_chunk*pl',InputFilePattern),
%	atom_concat_list(['sh parallel_predict.sh ', OutputFile, ' ', InputFilePattern], Cmd),
%	system(Cmd).
	

test :-
	annotate(['test_chunks.gen'],[], 'testout.pl').

% compute_and_save_annotations(++Stream,++ORF,++Type_Gene,++List_Ranges,++Dir,++Frame)
% Compute the annotation and write into Stream when a coding region is found.

compute_and_save_annotations(Stream_Out,_Nb_Iterations,[],[]) :-
        !,
        close(Stream_Out).

compute_and_save_annotations(Stream_Out,Nb_Iterations,[(Cp,Cn)|Rest_CpCn],[Range|Rest_Ranges]) :-
        check_or_fail(viterbiAnnot(cp_cn_voting(Cp,Cn,Annotation),_P),
                      error('Viterbi computation failed (Lost GeneFinder)')
                     ),
        build_term_for_annotation(cp_cn_voting,Range,Annotation,Term),
        (var(Term) ->
            true
        ;
            writeq(Stream_Out,Term),write(Stream_Out,'.'),nl(Stream_Out)
        ),
        Number is Nb_Iterations mod 100,
        (Number == 1 -> write(Nb_Iterations) ; write('.')),
        (Number == 0 ->
            table_remove(cp_cn_voting(_,_,_)),
            table_remove(cp_cn_recursicve(_,_,_,_))            
        ;
            true
        ),
        Nb_Iterations1 is Nb_Iterations+1,
        !,
        compute_and_save_annotations(Stream_Out,Nb_Iterations1,Rest_CpCn,Rest_Ranges) .
    



% build_term_for_annotation : Terms is a variable if the annotation is a list of 0, otherwise a term with a range for the coding region
build_term_for_annotation(Functor,[ID,Left,Right,Dir,Frame],AnnotationRev,Term) :-
        (Dir = '-' -> reverse(AnnotationRev,Annotation)
        ;
        Annotation = AnnotationRev),
        
        first_coding(Left,1,Annotation,Start),
        !,
        Term =..[ID,Functor,Left,Right,Dir,Frame,[cp_cn_voting(Annotation),start(Start)]].


build_term_for_annotation(_Functor,_Range,_Annotation,_Term).

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


parallel_learn([InputFile],[],ParamsFile) :-
        lost_tmp_directory(Tmp),
        atom_concat_list([Tmp,'cp_cn_voting_train.pl'],TrainingFile),
        make_training_file(InputFile,TrainingFile),
        split_file(TrainingFile,1000,'cpcn', '.pl',ResultingFiles),
        open('training_files.list',write,OutS),
        forall(member(File,ResultingFiles), (write(OutS,File), write(OutS,'\n'))),
        close(OutS),
        atom_concat_list(['./parallel_train.sh ','7 ', ParamsFile, ' training_files.list'], Cmd),
        system(Cmd).


% new verision using refseq'ed chunk in chunk_ref_file
% Multitrack file has all neccessary tracks for all chunks.
make_training_file(Multi_Track,Training_File):-
	open(Multi_Track,read,Chunk_Tracks_In,[alias(tracksIn)]),
	open(Training_File,write,Train_Stream,[alias(trainOut)]),
		
	read(Chunk_Tracks_In,Term),
	make_training_file_rec(0,Term, Chunk_Tracks_In, Train_Stream),!,	% green cut, for tail-recursion optimization  
	
	close(Train_Stream),
	close(Chunk_Tracks_In).


make_training_file_rec(Count,end_of_file, _Input_Stream, _Train_Stream):- nl, write('ok - '), write(Count), writeln(' training goals constructed').	

make_training_file_rec(Count,chunk(_Id,_Left,_Right,_Dir,_Frame,Extra), Input_Stream, Train_Stream):-
	member(codon_pref(Cp),Extra),
	member(blastgf(Cn),Extra),
	member(gb(Ref),Extra),
	NewCount is Count + 1,
	N is NewCount mod 500,
	(N = 0 -> write('.')
	; true
	),
	Training_Goal =.. [cp_cn_voting,Cp,Cn,Ref],
	writeq(Train_Stream,Training_Goal),	write(Train_Stream,'.'),
	nl(Train_Stream),
	read(Input_Stream,Next_Term),
	make_training_file_rec(NewCount, Next_Term, Input_Stream, Train_Stream).

	
