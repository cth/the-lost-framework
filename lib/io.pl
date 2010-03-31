%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  NAME :
%      io.pl
%
% VERSION :
%     0
%
% AUTHORS: Lost Members
%
% FUNCTION:
%      Manipulation of files: loading information from file, saving file from information
% 
% HISTORIC:
%  09/03: creation of file         MP
%
% REMARKS: any problem, contact {cth,otl,petit}@(without advertissement)ruc.dk
%
% MODULS USED: misc_utils.pl
%
% NOTE TO THE USER : 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%
% load_annotation_from_file(++Type_Info,++Options,++File,--Annotation)
%
% Description: given some Options, generate an Annotation from a set of terms contained into File
%
%%%%%%%%%%%%%%%%%%%%%%%%
% Type_Info: sequence
% Terms in File are composed of List of data.
% Annotation is a list
% Options available: Options = [data_position(Position),
%                               all_lists,range(Min,Max)]
% all_lists does not support a range option
%%%%%%%%%
load_annotation_from_file(sequence,Options,File,Annotation) :-
        terms_from_file(File,Terms),
        % Technically not necessary since they will be sorted if this
	% interface is used
	sort('=<',Terms,SortedTerms), 
        sequence_terms_to_annotations(Options,SortedTerms,Annotation).


%%%%%%%%%%%%%%%%%%%%
% Type: db
% Terms in File are composed of Range that delimites a specific region.
% Options available: Options = [in_db(Letter),not_in_db(Letter),
%                               range_position(Position),range(Min,Max)]
% Note: assumption is done on the format of the database. Information is extracted from a list
% of terms that have a parameter Range to describe a specific region of the genome.
% By defaut, annotation of the specific region is 1 and 0 when the region is not specific.
%
%%%%%%%%%%%%%%%%%%%%%


load_annotation_from_file(db,Options,File,Annotation) :-
        terms_from_file(File,Terms),
        % Technically not necessary since they will be sorted if this
	% interface is used
	sort('=<',Terms,SortedTerms), 
        db_terms_to_annotations(Options,SortedTerms,Annotation).

% Utils for load_annotation_from_file(sequence,....)

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

% Options all_lists (Note: note Range option not support)
% Options Data Position
sequence_terms_to_annotations(Options,[Data|Data_Terms],[Sequence_Data|Annotation]) :-
        member(all_lists,Options),
        member(data_position(Data_Position),Options),
        !,
        Data =.. [_Functor|Rest_Data],
        nth1(Data_Position,Rest_Data,Sequence_Data),
        sequence_terms_to_annotations(Options,Data_Terms,Annotation).

% Options all_lists (Note: note Range option not support)
% Options Data Position
sequence_terms_to_annotations(Options,[Data|Data_Terms],[Sequence_Data|Annotation]) :-
        member(all_lists,Options),
        !,
        Data =.. [_Functor,_,Sequence_Data|_Rest_Data],
        sequence_terms_to_annotations(Options,Data_Terms,Annotation).



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







% Utils for load_annotation_from_file(sequence)

%%%
% db_terms_annotations(++Options,++List_Terms,--Annotation)
%%%
%Options = [in_db(Letter),out_db(Letter),range_position(Param_Start,Param_End),range(Min,Max)]


db_terms_to_annotations(_Options,[],[]) :-
        !.


db_terms_to_annotations(Options,[DB|List_Terms],Annotation) :-
        member(range(Min,Max),Options),
        !,
        init_db_terms(Options,Annot_Format,Range_Position),
        get_next_range(Range_Position,DB,Range),
        db_terms_to_annotations_rec(1,range(Min,Max),Range,Annot_Format,Range_Position,List_Terms,Annotation).



db_terms_to_annotations(Options,[DB|List_Terms],Annotation) :-
        init_db_terms(Options,Annot_Format,Range_Position),
        get_next_range(Range_Position,DB,Range),
        db_terms_to_annotations_rec(1,not_range,Range,Annot_Format,Range_Position,List_Terms,Annotation).


% Recursive call
% Options: No range(Min,Max) 

% When no range is specificied, annotation stops when the last Position = Max for the last Range of the db
db_terms_to_annotations_rec(Position,not_range,(_,Max),_Annot_Format,_Range_Position,[],[]) :-
        Position > Max,
        !.

% End of a Range, 
db_terms_to_annotations_rec(Position,not_range,(_Min,Max),Annot_Format,Range_Position,[DB|List_Terms],Annotations) :-
        Position > Max,
        !,
        get_next_range(Range_Position,DB,New_Range),
        db_terms_to_annotations_rec(Position,not_range,New_Range,Annot_Format,Range_Position,List_Terms,Annotations).

% Outside a specific Region 
db_terms_to_annotations_rec(Position,not_range,(Min,Max),(Letter_Out,Letter_In),Range_Position,List_Terms,[Letter_Out|Annotations]) :-
        Position < Min,
        !,
        Next_Position is Position+1,
        db_terms_to_annotations_rec(Next_Position,not_range,(Min,Max),(Letter_Out,Letter_In),Range_Position,List_Terms,Annotations).


% Inside a specific Region 
db_terms_to_annotations_rec(Position,not_range,(Min,Max),(Letter_Out,Letter_In),Range_Position,List_Terms,[Letter_In|Annotations]) :-
        Position >= Min,
        Position =< Max,
        !,
        Next_Position is Position+1,
        db_terms_to_annotations_rec(Next_Position,not_range,(Min,Max),(Letter_Out,Letter_In),Range_Position,List_Terms,Annotations).


% Recursive call
% Options: range(Range_Min,Range_Max) 

% Ends when Position = Max_Range
db_terms_to_annotations_rec(Position,range(_Range_Min,Range_Max),_Range,_Annot_Format,_Range_Position,_List_Terms,[]) :-
        Position > Range_Max,
        !.


% End of the list of terms, but outside the specified range
db_terms_to_annotations_rec(Position,range(Range_Min,Range_Max),(_Min,Max),(Letter_Out,Letter_In),_Range_Position,[],Annotations) :-
        Position > Max,
        Position < Range_Min,
        !,
        New_Position = Range_Min,
        db_terms_to_annotations_rec(New_Position,range(Range_Min,Range_Max),(_Min,Max),(Letter_Out,Letter_In),_Range_Position,[],Annotations).


% End of the list of terms, but inside the specified range
db_terms_to_annotations_rec(Position,range(Range_Min,Range_Max),(_Min,Max),(Letter_Out,_Letter_In),_Range_Position,[],[Letter_Out|Annotations]) :-
        Position > Max,
        Position >= Range_Min,
        Position =< Range_Max,
        !,
        New_Position is Position+1,
        db_terms_to_annotations_rec(New_Position,range(Range_Min,Range_Max),(_Min,Max),(Letter_Out,_Letter_In),_Range_Position,[],Annotations).



% End of a DB range update by a new one
db_terms_to_annotations_rec(Position,range(Range_Min,Range_Max),(_Min,Max),Annot_Format,Range_Position,[DB|List_Terms],Annotations) :-
        Position > Max,
        !,
        get_next_range(Range_Position,DB,New_Range),
        db_terms_to_annotations_rec(Position,range(Range_Min,Range_Max),New_Range,Annot_Format,Range_Position,List_Terms,Annotations).


% No generation: outside the specified range + update of the new DB range
db_terms_to_annotations_rec(Position,range(Range_Min,Range_Max),(Min,Max),_Annot_Format,_Range_Position,_List_Terms,Annotations) :-
        Position < Range_Min,
        !,
        update_position_jump(Range_Min,(Min,Max),New_Position),
        db_terms_to_annotations_rec(New_Position,range(Range_Min,Range_Max),(Min,Max),_Annot_Format,_Range_Position,_List_Terms,Annotations).


% Inside the specified range but outside a specific region
db_terms_to_annotations_rec(Position,range(Range_Min,Range_Max),(Min,_Max),(Letter_Out,_Letter_In),_Range_Position,_List_Terms,[Letter_Out|Annotations]) :-
        Position >= Range_Min,
        Position =< Range_Max,
        Position < Min,
        !,
        New_Position is Position+1,
        db_terms_to_annotations_rec(New_Position,range(Range_Min,Range_Max),(Min,_Max),(Letter_Out,_Letter_In),_Range_Position,_List_Terms,Annotations).


% Inside the specified range but inside a specific region
db_terms_to_annotations_rec(Position,range(Range_Min,Range_Max),(Min,Max),(Letter_Out,Letter_In),Range_Position,List_Terms,[Letter_In|Annotations]) :-
        Position >= Range_Min,
        Position =< Range_Max,
        Position >= Min,
        Position =< Max,
        !,
        New_Position is Position+1,
        db_terms_to_annotations_rec(New_Position,range(Range_Min,Range_Max),(Min,Max),(Letter_Out,Letter_In),Range_Position,List_Terms,Annotations).


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
            (member(out_db(Letter_Out),Options) ->
                Annot_Format = (Letter_Out,1)
            ;
                Annot_Format = (0,1)
            )
        ),
        (member(range_position(Param_Start,Param_End),Options) ->
            Range_Position = (Param_Start,Param_End)
        ;
            Range_Position = (1,2)
        ).



% get_next_range(++Range_Position,++DB,--Range)
get_next_range((Param_Start,Param_End),DB,(Min,Max)) :-
        DB =.. [_|List_Params],
        nth1(Param_Start,List_Params,Min),
        nth1(Param_End,List_Params,Max).


% update_position_jump(++Range_Min,(++Min,++Max),-Position)
update_position_jump(Range_Min,(_Min,Max),New_Position) :-
        Range_Min < Max,
        !,
        New_Position = Range_Min.


% update_position_jump(++Range_Min,(++Min,++Max),-Position)
update_position_jump(_Range_Min,(_Min,Max),New_Position) :-
        New_Position is Max+1.



%--------------------------------
% Saving information to file  %
%--------------------------------

% save_sequence_list_to_file(++File,--Sequence)

save_annotation_to_sequence_file(KeyIndex,ChunkSize,Annotation,File) :-
	split_list_in_chunks(ChunkSize,Annotation,DataChunks),
	create_sequence_terms(KeyIndex,1,DataChunks,Terms),
	terms_to_file(File,Terms).

create_sequence_terms(_,_,[],[]).

create_sequence_terms(KeyIndex,StartPos,[Chunk|ChunksRest],[Term|TermsRest]) :-
	length(Chunk,ChunkLen),
	EndPos is StartPos + ChunkLen - 1,
	NextStartPos is EndPos + 1,
	Term =.. [ data, KeyIndex, StartPos, EndPos, Chunk ],
	create_sequence_terms(KeyIndex,NextStartPos,ChunksRest,TermsRest).

split_list_in_chunks(_,[],[]).

split_list_in_chunks(ChunkSize, List, [Chunk|ChunksRest]) :-
	nfirst_list(ChunkSize,List,Chunk,RestList),
	!,
	split_list_in_chunks(ChunkSize,RestList,ChunksRest).

split_list_in_chunks(ChunkSize, List, [List]) :-
	length(List,ListLength),
	ListLength < ChunkSize.

nfirst_list(0,L,[],L).

nfirst_list(N,[E|List],[E|NFirstList],RestList) :-
	N1 is N - 1,
	nfirst_list(N1,List,NFirstList,RestList).

save_sequence_list_to_file(File,Sequence) :-
	data_elements_to_sequence_terms(Sequence,Terms),
	terms_to_file(File,Terms).

data_elements_to_sequence_terms(Data,Terms) :-
	data_elements_to_sequence_terms(1,Data,Terms).

data_elements_to_sequence_terms(_,[],[]).
data_elements_to_sequence_terms(Pos,[Data|R1],[elem(Pos,Data)|R2]) :-
	NextPos is Pos + 1,
	data_elements_to_sequence_terms(NextPos,R1,R2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Basic reading/writing of terms to/from file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% terms_from_file(++File,--Terms)
% Reads all Terms from named file File
terms_from_file(File, Terms) :-
	open(File, read, Stream),
	ground(Stream),
	collect_stream_terms(Stream,Terms),
	close(Stream).

% terms_to_file(++File,++Terms)
% Writes all Terms to named file File
terms_to_file(File,Terms) :-
	open(File,write,Stream),
	ground(Stream),
	write_terms_to_stream(Stream,Terms),
	close(Stream).

% Writes terms to a Stream
write_terms_to_stream(_,[]).
write_terms_to_stream(Stream,[Term|Rest]) :-
	writeq(Stream,Term),
	write(Stream,'.\n'),
	write_terms_to_stream(Stream,Rest).

% Create list of Rules found in Stream
collect_stream_terms(Stream, Rules) :-
	read(Stream, T),
	((T == end_of_file) ->
		Rules = []
	;
		collect_stream_terms(Stream,Rest),
		append([T],Rest,Rules)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Utility for finding the functor in a text(prolog(_)) format
% Eg. the file is expected to have only one functor
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

file_functor(Filename, Functor) :-
	terms_from_file(Filename,Terms),
	findall( F,((member(X,Terms), X =.. [ F |_ ])), Functors),
	eliminate_duplicate(Functors,[Functor]).
		 
		