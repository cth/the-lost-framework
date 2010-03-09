%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Rudimentary management of sequence data 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Asssuming that the file contains facts on the form:
% elem(1,...), elem(2,...) etc.
load_sequence_list_from_file(File,Sequence) :-
	terms_from_file(File,Terms),
	% Technically not necessary since they will be sorted if this
        % interface is used
	sort('=<',Terms,SortedTerms), 
	sequence_terms_to_data_elements(SortedTerms,Sequence).

% sequence_terms_to_data_elements(++ Data_Terms,--Sequence_Data)

sequence_terms_to_data_elements([],[]).
sequence_terms_to_data_elements([elem(_,Data)|R1],[Data|R2]) :-
	sequence_terms_to_data_elements(R1,R2).


data_elements_to_sequence_terms(Data,Terms) :-
	data_elements_to_sequence_terms(1,Data,Terms).

data_elements_to_sequence_terms(_,[],[]).
data_elements_to_sequence_terms(Pos,[Data|R1],[elem(Pos,Data)|R2]) :-
	NextPos is Pos + 1,
	data_elements_to_sequence_terms(NextPos,R1,R2).


% save_sequence_list_to_file(++File,--Sequence)

save_sequence_list_to_file(File,Sequence) :-
	data_elements_to_sequence_terms(Sequence,Terms),
	terms_to_file(File,Terms).



% Type: sequence
% Options available: Options = [data_position(Position),
%                               range(Min,Max)]

load_annotation_from_file(sequence,Options,File,Annotation) :-
        terms_from_file(File,Terms),
        % Technically not necessary since they will be sorted if this
	% interface is used
	sort('=<',Terms,SortedTerms), 
        sequence_terms_to_annotations(Options,SortedTerms,Annotation).



% Type: db
% Options available: Options = [in_db(Letter),not_in_db(Letter),
%                               data_position(Position),range(Min,Max)]

load_annotation_from_file(db,Options,File,Annotation) :-
        terms_from_file(File,Terms),
        % Technically not necessary since they will be sorted if this
	% interface is used
	sort('=<',Terms,SortedTerms), 
        db_terms_to_annotations(Options,SortedTerms,Annotation).


%%%
% sequence_terms_annotations(++Options,++List_Terms,--Annotation)
%%%
% Default: Functor(1,Data),Functor(2,Data) ....
sequence_terms_to_annotations([],[],[]) :-
        !.


sequence_terms_to_annotations([],[Data|Rest_Data],Annotation) :-
        !,
        sequence_terms_to_annotations([],Rest_Data,Rest_Annotation),
        Data =.. [_Functor,_,Sequence_Data|_], 
        append(Sequence_Data,Rest_Annotation,Annotation).


% Options Range and Data Position
sequence_terms_to_annotations(_Options,[],[]) :-
        !.
     
% Options Range and Data Position
sequence_terms_to_annotations(Options,[Data|Data_Terms],Annotation) :-
        member(range(Min,Max),Options),
        member(data_position(Data_Position),Options),
        !,
        Data =.. [_Functor|Rest_Data],
        nth1(Data_Position,Rest_Data,Sequence_Data),
        sequence_terms_to_annotations_rec(Sequence_Data,range(Min,Max),1,Data_Position,Data_Terms,Annotation).


% Options Range only
sequence_terms_to_annotations(Options,[Data|Data_Terms],Annotation) :-
        member(range(Min,Max),Options),
        !,
        Data =.. [_Functor,_,Sequence_Data|_],  % Default functor(Num,Sequence_Data,...)
        sequence_terms_to_annotations_rec(Sequence_Data,range(Min,Max),1,2,Data_Terms,Annotation).


% Options Data Position only
sequence_terms_to_annotations(Options,[Data|Data_Terms],Annotation) :-
        member(data_position(Data_Position),Options),
        !,
        Data =.. [_Functor|Rest_Data],
        nth1(Data_Position,Rest_Data,Sequence_Data),
        sequence_terms_to_annotations(Options,Data_Terms,Rest_Annotation),
        append(Sequence_Data,Rest_Annotation,Annotation).


% Data_Terms empty = end of the annotation generation
sequence_terms_to_annotations_rec([],range(_Min,_Max),_Position,_Data_Position,[],[]) :-
        !.


% Recursive call used when range option is asked

% End of the annotation
sequence_terms_to_annotations_rec(_Sequence_Data,range(_Min,Max),Position,_Data_Position,_Data_Terms,[]) :-
        Position > Max,
        !.

% Sequence_Data Empty
sequence_terms_to_annotations_rec([],range(Min,Max),Position,Data_Position,[Data|Data_Terms],Annotation) :-
      Position =< Max,
      !,
      Data =.. [_Functor|Rest_Data], 
      nth1(Data_Position,Rest_Data,Sequence_Data),
      sequence_terms_to_annotations_rec(Sequence_Data,range(Min,Max),Position,Data_Position,Data_Terms,Annotation).

% Parse of Min data      
sequence_terms_to_annotations_rec([_|Rest_Sequence_Data],range(Min,Max),Position,Data_Position,Data_Terms,Annotation) :-
      Position < Min,
      !,
      Position1 is Position+1,
      sequence_terms_to_annotations_rec(Rest_Sequence_Data,range(Min,Max),Position1,Data_Position,Data_Terms,Annotation).


sequence_terms_to_annotations_rec([Annot|Rest_Sequence_Data],range(Min,Max),Position,Data_Position,Data_Terms,[Annot|Annotation]) :-
      Position >= Min,
      Position =< Max,
      !,
      Position1 is Position+1,
      sequence_terms_to_annotations_rec(Rest_Sequence_Data,range(Min,Max),Position1,Data_Position,Data_Terms,Annotation).





%%%
% db_terms_annotations(++Options,++List_Terms,--Annotation)
%%%
%Options = [in_db(Letter),out_db(Letter),range_position(Param_Start,Param_End),range(Min,Max)]
% Note: assumption is done on the format of the database. Information is extracted from a list
% of terms that have a parameter Range to describe a specific region of the genome.
% By defaut, annotation of the specific region is 1 and 0 when the region is not specific.



db_terms_to_annotations(Options,[],[]) :-
        !.


db_terms_to_annotations(Options,[DB|List_Terms],Annotation) :-
        member(range(Min,Max),Options),
        !,
        init_db_terms(Options,Annot_Format,Range_Position),
        get_next_range(Range_Position,DB,Range)
        db_terms_to_annotations_rec(Options,1,range(Min,Max),Range,Annot_Format,Range_Position,List_Terms,Annotation).



db_terms_to_annotations(Options,[DB|List_Terms],Annotation) :-
        init_db_terms(Options,Annot_Format,Range_Position),
        get_next_range(Range_Position,DB,Range)
        db_terms_to_annotations_rec(Options,Range,Annot_Format,Range_Position,List_Terms,Annotation).


% Recursive call
% Options: No range(Min,Max) 


%%%
% Utils db_terms_to_annotations
%%%

% Setting of Annot_Format and Range_Position
% Default(when non member) Annot_Format = (0,1), Range_Position = (1,2)
init_db_terms(Options,Annot_Format,Range_Position) :-
        (member(in_db(Letter_In),Options) ->
            (member(out_db(Letter_Out),Options) ->
                Annot_Format = (Letter_Out,Letter_In)
            ;
                Annot_Format = (0,Letter_In)
            )
        ;
            Annot_Format = (0,1)
        ),
        (member(range_position(Param_Start,Param_End),Options) ->
            Range_Position = (Param_Start,Param_End)
        ;
            Range_Position = (1,2)
        ).



% get_next_range(++Range_Position,++DB,--Range)
get_next_range(Param_Start,Param_End,DB,(Min,Max)) :-
        DB =.. [_|List_Params],
        nth1(Param_Start,List_Params,Min),
        nth1(Param_End,List_Params,Max).

