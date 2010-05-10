% Script to used the lost_genefinder: HMM based on a duration model


:- ['../lost.pl'].
:- lost_include_api(misc_utils).
:- lost_include_api(interface).
:- lost_include_api(stats).
%:- ['../tmp/hard_to_find.pl'].
%:- ['../tmp/orf_U00096_+1.pl'].
%:- ['../data/gb_fragment.seq'].

%----------
% Learning:
%      Step1: ORF chopping from Ole
%      Step2: Select N ORF detected as hard or medium or easy gene
%      Step3: Select N ORF known as not coding
%      Step4: Generate the correct annotation
%      Step5: Learn from PRISM
%---------
script_learning :-
        get_coding_ORF(Data_Coding,Starts_Position),
        get_non_coding_ORF(Data_nonCoding),
        generation_annotation_coding(Data_Coding,Starts_Position,Annotation_Coding),
        build_annotation_noncoding(Data_nonCoding,Annotation_Non_Coding),
        map(build_learning_terms,Annotation_Coding,Terms_Coding),
        map(build_learning_terms,Annotation_Non_Coding,Terms_Non_Coding),
        pick_a_given_number(400,Terms_Non_Coding,Terms_Non_Coding2),
        append(Terms_Coding,Terms_Non_Coding2,Terms_Learning),
        prism('../models/lost_genefinder/lost_genefinder.psm'),
        learn(Terms_Learning),
        check_or_fail(
		      save_sw('../models/lost_genefinder/parameters/test_hard.prb'),
		      error(could_not_save_parameter_to_file(_ParametersFile))).
        
        





%---
% utils script learning
%---

get_coding_ORF(Data_Coding,Starts_Position) :-
        findall([Data,(Min,Starts_Position)],(chunk(_,Min,Max,Data,Dir,_,_,_),hard_to_find(Starts_Position,Max,Dir,_)),Result),
        divide_list(Result,[Data_Coding,Starts_Position]).



get_non_coding_ORF(Data_nonCoding) :-
        findall(Max,gb(_,Max,_,_,_),Res),
        findall(Data,(chunk(_,_Min,Max1,Data,_,_,_,_),not_member(Max1,Res)),Data_nonCoding).
        
        
% Predicates usefull combine with a map
build_learning_terms(Params,Term) :-
        Term =.. [hmm_lost|Params].


divide_list([],[[],[]]) :-
        !.

divide_list([[A,B]|Rest],[[A|Rest_A],[B|Rest_B]]) :-
        divide_list(Rest,[Rest_A,Rest_B]).



generation_annotation_coding([],[],[]) :-
        !.
 
generation_annotation_coding([Data|Data_Coding],[(Start_ORF,Start_Coding)|Starts_Position],[[Data,Annotation]|Rest_Annotation]) :-
        generate_list_annotation(Data,Start_ORF,Start_Coding,Annotation),
        generation_annotation_coding(Data_Coding,Starts_Position,Rest_Annotation).
        
        
generate_list_annotation([],_Start_ORF,_Start_Coding,[]) :-
        !.


generate_list_annotation([_N|Rest_Data],Start_ORF,Start_Coding,[1|Rest_Annot]) :-
        Start_ORF >= Start_Coding,
        !,
        generate_list_annotation(Rest_Data,Start_ORF,Start_Coding,Rest_Annot).
        

generate_list_annotation([_N|Rest_Data],Start_ORF,Start_Coding,[0|Rest_Annot]) :-
        Start_ORF1 is Start_ORF+1,
        generate_list_annotation(Rest_Data,Start_ORF1,Start_Coding,Rest_Annot).


build_annotation_noncoding([],[]) :-
        !.

build_annotation_noncoding([Data|Data_nonCoding],[[Data,Annotation]|Annotation_Non_Coding]) :-
        generate_list_noncoding(Data,Annotation),
        build_annotation_noncoding(Data_nonCoding,Annotation_Non_Coding).



generate_list_noncoding([],[]) :-
        !.

generate_list_noncoding([_V|Rest_Data],[0|Rest_Annotation]) :-
        generate_list_noncoding(Rest_Data,Rest_Annotation).


pick_a_given_number(0,_,[]) :-
        !.

pick_a_given_number(Num,[Elt|Rest],[Elt|Rest_Res]) :-
        !,
        Num1 is Num-1,
        pick_a_given_number(Num1,Rest,Rest_Res).


%--------------
% Annotation:
%      Step1: Input ORF chopping from Ole
%      Step2: Annotated all the ORF ()
%
%
%-------------

script_annotation(ChunkFile_Name,Type_Gene,Result_File) :-
        lost_sequence_file(ChunkFile_Name,ChunkFile),
        get_annotation_file(lost_genefinder,
			    [ChunkFile],
			    [type_gene(Type_Gene)], % Options Gene_Type
			    Result_File),
        write('Lost annotations succeed!!'),nl.
        


        

