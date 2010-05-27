:- ['../../lost.pl'].                                                                   
:- lost_include_api(interface).                                                         
:- lost_include_api(autoAnnotations).
:- lost_include_api(misc_utils).
:- lost_include_api(io).                                                                                                                                        
:- lost_include_api(stats).

% Option declaration
lost_option(lost_best_annotation,type_gene,hard,'Specified with which kind of data the genefinder is set').
lost_option(lost_best_annotation,use_parameter_file,yes,'Load parameters from the parameter file').
lost_option(lost_best_annotation,optimized,false,'whether to split prediction in multiple processes').

% Input Format Specification
lost_input_formats(lost_best_annotation,[prolog(sequence(_))]).
% Output Format Specification
lost_output_format(lost_best_annotation,_,text(prolog(ranges(_)))).



% 
lost_best_annotation([InputFile],Options,OutputFile) :-                                 
	write('Lost genefinder: '),nl,                                                         
        prismAnnot('lost_genefinder'), % Load the actual PRISM model                                         
        get_option(Options,use_parameter_file,UseParamFile),
        get_option(Options,type_gene,Type_Gene),
	get_option(Options,optimized,false),
        (UseParamFile == yes ->
            atom_concat('./Parameters/',Type_Gene,Path),
            atom_concat(Path,'.prb',ParamFile),
            write(restore_sw(ParamFile)),nl,
            restore_sw(ParamFile)
        ;
            true),
       % Building of the Input for the annotations
        consult(InputFile),
        chunk(_Key,_Min,_Max,_Data,Dir,Frame,_,_),
        findall(Data,chunk(_Key,_,_,Data,_,_,_,_),List_ORF),
        findall([Min,Max],chunk(_Key,Min,Max,_,_,_,_,_),List_Ranges_ORF),
        % Computation of annotations
        open(OutputFile,write,Stream_Out),
        compute_and_save_annotations(Stream_Out,1,List_ORF,Type_Gene,List_Ranges_ORF,Dir,Frame).
        % Save the annotations in the right format
        %write(save_annotation(lost_prediction,List_Ranges_ORF,List_Annotations,Dir,Frame,OutputFile)),nl,
        %save_annotation(lost_prediction,List_Ranges_ORF,Dir,Frame,List_Annotations,OutputFile). % Måske, something more generic to include in io 
        %save_annotation_to_sequence_file(genemark_genefinder,70,Annotation,OutputFile).

lost_best_annotation([InputFile],Options,OutputFile) :-
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
	

test :-
	lost_best_annotation(['testdata.pl'],[optimized(true),type_gene(medium), use_parameter_file(yes)], 'testout.pl').

% compute_and_save_annotations(++Stream,++ORF,++Type_Gene,++List_Ranges,++Dir,++Frame)
% Compute the annotation and write into Stream when a coding region is found.

compute_and_save_annotations(Stream_Out,_Nb_Iterations,[],_Type_Gene,[],_Dir,_Frame) :-
        !,
        close(Stream_Out).

compute_and_save_annotations(Stream_Out,Nb_Iterations,[ORF|Rest_ORF],Type_Gene,[Range|Rest_Ranges],Dir,Frame) :-
        check_or_fail(viterbiAnnot(hmm_lost_annot(ORF,Type_Gene,Annotation),_P),
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


