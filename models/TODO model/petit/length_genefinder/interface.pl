%-----
% TODO list:
%     Update interface.pl: I hope same interface for several probabilistic models



:- ['../../lost.pl'].
:- lost_include_api(autoAnnotations).
%:- lost_include_api(chmm).
:- lost_include_api(interface).
:- lost_include_api(io).                                                                                                                                        
:- lost_include_api(misc_utils).
:- lost_include_api(stats).
:- ['./data/domain_U00096.pl'].
:- ['./data/stats_length_U00096.pl'].
%:- ['./constraints/constraints.pl'].

:- lost_include_script(consorf).

% Input Format Specification
lost_input_formats(annotate,[prolog(sequence(_))]).
lost_input_formats(learn,[]). % TODO
% Output Format Specification
lost_output_format(annotate,_,text(prolog(ranges(_)))).
lost_output_format(train, _, text(prolog(prism_swithches))).


% Option declaration annotate
lost_option(annotate,type_duration,all_genome,'Specified with which kind of genes the duration is modeled').
lost_option(annotate,use_parameter_file,yes,'Load parameters from the parameter file').
lost_option(annotate,optimized,false,'whether to split prediction in multiple processes').  % Option disable
lost_option(annotate,type_duration,all_genome,'Specified with which kind of genes the duration is modeled').
% Option declaration learn
lost_option(train,term_numbers,200,'Specified how many ORF are use to learn').
lost_option(train,type_duration,all_genome,'Specified with which kind of genes the duration is modeled').



%-------
% annotate
%-------
annotate([InputFile],Options,OutputFile) :-                                 
	write('Length genefinder: '),nl,                                                         
        prismAnnot('length_genefinder'), % Load the actual PRISM model                                         
        get_option(Options,use_parameter_file,UseParamFile),
        get_option(Options,type_duration,Duration_Type),
%%%	get_option(Options,optimized,false),
%%%        (UseParamFile == yes ->
%%%            atom_concat('./Parameters/',Type_Gene,Path),
%%%            atom_concat(Path,'.prb',ParamFile),
%%%            write(restore_sw(ParamFile)),nl,
%%%            restore_sw(ParamFile)
%%%        ;
%%%            true),
       % Building of the Input for the annotations
        consult(InputFile),
        chunk(_Key,_Min,_Max,Dir,Frame,_),
        findall(Data,(chunk(_Key,_,_,_,_,Info),member(sequence(Data),Info)),List_ORF),
        findall([Min,Max],chunk(_Key,Min,Max,_,_,_),List_Ranges_ORF),
        % Computation of annotations
        open(OutputFile,write,Stream_Out),
        compute_and_save_annotations(Stream_Out,1,List_ORF,Duration_Type,List_Ranges_ORF,Dir,Frame).
        % Save the annotations in the right format
        %write(save_annotation(lost_prediction,List_Ranges_ORF,List_Annotations,Dir,Frame,OutputFile)),nl,
        %save_annotation(lost_prediction,List_Ranges_ORF,Dir,Frame,List_Annotations,OutputFile). % Måske, something more generic to include in io 
        %save_annotation_to_sequence_file(genemark_genefinder,70,Annotation,OutputFile).

annotate([InputFile],Options,OutputFile) :-
	get_option(Options,optimized,true),
	subtract(Options,[optimized(true)],NewOptions1),
	append([optimized(false)], NewOptions1, NewOptions2),
	terms_to_file('options.pl',[original_options(NewOptions2)]),
	lost_tmp_directory(Tmp),
	atom_concat(Tmp,'lost_genefinder_chunk',Prefix),
	split_file(InputFile, 10, Prefix, '.pl'),
	atom_concat(Tmp,'lost_genefinder_chunk*pl',InputFilePattern),
	atom_concat_list(['sh parallel_predict.sh ', OutputFile, ' ', InputFilePattern], Cmd),
	system(Cmd).
	

% 
train([GB_Filtered,RawGenome],Options,OutputFile) :-
        % Computation of the different ORF of the genome
        run_model(orf_chopper,annotate([RawGenone],[minimal_length(30),frame(1),direction(+)],OrfChunk_File_Plus1)),   
        run_model(orf_chopper,annotate([RawGenone],[minimal_length(30),frame(2),direction(+)],OrfChunk_File_Plus2)),
        run_model(orf_chopper,annotate([RawGenone],[minimal_length(30),frame(3),direction(+)],OrfChunk_File_Plus3)),
        run_model(orf_chopper,annotate([RawGenone],[minimal_length(30),frame(1),direction(-)],OrfChunk_File_Minus1)),
        run_model(orf_chopper,annotate([RawGenone],[minimal_length(30),frame(2),direction(-)],OrfChunk_File_Minus2)),
        run_model(orf_chopper,annotate([RawGenone],[minimal_length(30),frame(3),direction(-)],OrfChunk_File_Minus3)),

        get_coding_ORF(Data_Coding,Starts_Position),
        get_non_coding_ORF(Data_nonCoding),
        generation_annotation_coding(Data_Coding,Starts_Position,Gene_Type,Annotation_Coding),
        build_annotation_noncoding(Data_nonCoding,Gene_Type,Annotation_Non_Coding),
        map(build_learning_terms,Annotation_Coding,Terms_Coding),
        map(build_learning_terms,Annotation_Non_Coding,Terms_Non_Coding),
        pick_a_given_number(400,Terms_Coding,Terms_Coding2),
        pick_a_given_number(400,Terms_Non_Coding,Terms_Non_Coding2),
        append(Terms_Coding2,Terms_Non_Coding2,Terms_Learning),
        prism('../models/lost_genefinder/lost_genefinder.psm'),
        learn(Terms_Learning),
        atom_concat('../models/lost_genefinder/Parameters/',Gene_Type,Proba_File),
        atom_concat(Proba_File,'.prb',Proba_File2),
        check_or_fail(
		      save_sw(Proba_File2),
		      error(could_not_save_parameter_to_file(_ParametersFile))).
        




% compute_and_save_annotations(++Stream,++ORF,++Type_Gene,++List_Ranges,++Dir,++Frame)
% Compute the annotation and write into Stream when a coding region is found.

compute_and_save_annotations(Stream_Out,_Nb_Iterations,[],_Type_Gene,[],_Dir,_Frame) :-
        !,
        close(Stream_Out).

compute_and_save_annotations(Stream_Out,Nb_Iterations,[ORF|Rest_ORF],Type_Gene,[Range|Rest_Ranges],Dir,Frame) :-
        check_or_fail(viterbiAnnot(hmm_length(ORF,Type_Gene,Annotation),_P),
                      error('Viterbi computation failed (Lost GeneFinder)')
                     ),
        build_term_for_annotation(lost_prediction,Range,Dir,Frame,Annotation,Term),
        (var(Term) ->
            true
        ;
            write(Stream_Out,Term),write(Stream_Out,'.'),nl(Stream_Out)
        ),
        Number is Nb_Iterations mod 100,
        (Number == 1 -> write(Nb_Iterations) ; write('.')),
        (Number == 0 ->
            table_remove(hmm_lost_annot(_,_)),
            table_remove(hmm_lost_annot(_,_,_))
        ;
            true
        ),
        Nb_Iterations1 is Nb_Iterations+1,
        !,
        compute_and_save_annotations(Stream_Out,Nb_Iterations1,Rest_ORF,Type_Gene,Rest_Ranges,Dir,Frame) .
    



% build_term_for_annotation : Terms is a variable if the annotation is a list of 0, otherwise a term with a range for the coding region
build_term_for_annotation(Functor,[Left,Right],Dir,Frame,Annotation,Term) :-
        first_coding(Left,1,Annotation,Start),
        !,
        Term =..[Functor,Left,Right,Dir,Frame,[lost_annotation(Annotation),start(Start)]].


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

%%%divided_file_by_frame() :-
%%%        findall([data,Min,Max,'+',Data],(gene(Min,Max,'+',Data),get_frame(Min,'+',1)),Result_Frame1)
        





%%%get_frame(Min,'+',Frame) :-
%%%        !,
%%%        Frame is Min mod 3.
      


%%%get_frame(Min,'-',Frame) :-
%%%        !,
%%%        Frame1 is Min mod 3,
%%%        (Frame1 = 3 ->
%%%            Frame = 0
%%%        ;
%%%            Frame1 = Frame
%%%        ).


