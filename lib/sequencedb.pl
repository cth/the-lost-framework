:- module(sequencedb,[sequence_from_file/3,sequence_to_file/4]).

/** <module> sequencedb 

This library contains predicates for working with files of the =|text(prolog(sequence(_any)))|=.

The typical =|prolog(sequence(_))|= file contains facts,
==
data(somekey, 1, 4, [a,b,c,d]).
data(somekey, 5, 8, [e,f,g,h]).
==

The size of list may vary and is often tweaked for the efficiency of applications, e.g. 
in the extreme case the file will only contain one fact where the fourth argument is a long list.
This, may however be quite inefficient to access. 

Often, these files may even be prohibitively large to consult and predicates for accessing 
the files without consulting are necessary. 

The predicate =|seqdb_data_from_file|= address this problem by accessing the file in 
a sequential fashion and extracting data from one fact at a time. 

If many repeated queries are going to be made to the file, then repeated sequential access 
may be to slow. If the file is not to big to fit in memory then loaded into memory and accessed
via the predicate, =|seqdb_memory_map_file|=. Then, the predicate get_sequence_range/4 can then be used
to extract a part of the sequence. This is fairly efficient, but depends on the size of the data terms, 
|t|, in the file. The running time of the predicate is bounded by O(2|t| × n), where n = Max − Min.

@author: Matthieu Petit and Christian Theil Have

*/


%% sequence_from_file(+File,+Options,-Data)
%  This predicate aims at efficiently extracting data (for instance, a list of nucleotides) from a file 
% with a format =|text(prolog(sequence(_any)))|=.
% Given of file composed of prolog facts, this predicate generates a list, Data. 
% By default, data predicate is in the form but the predicate is able to handle
% slightly different formats as well.
% ==
% Functor(Key,LeftPosition,RightPosition,Data,...)
% ==
% Type of Data is a list
%
% Options: - data_position(Pos): Where Pos is an integer that specifies the argument that contains the list with of data, i.e. it is 4, meaning the fourth argument.
%          - left_position(Left): Left is and integer that specifies the argument of which holds Leftposition. If left unspecified, it has default value 2. If the term has no argument to indicate left position, then =|left_position(none)|= should be used.
%          - right_position(Right): Right is and integer that specifies the argument of which holds Rightposition. If left unspecified, it has default value 3. If the term has no argument to indicate right position, then =|right_position(none)|= should be used.
%
% By default, the predicate extracts and collects in Data all the data available in File.
% However, two options are available to ask for partial information:
% 
%          - range(Min,Max): extract a range of data
%          - ranges(List_Ranges): extract a list of data given a list of Range
%
% Note that data extraction is made without taking care about the strand (for a DNA sequence). 
% 
% @author: Matthieu Petit
sequence_from_file(File,Options,Data) :-
        terms_from_file(File,Terms),  % An other way could be to consult the file (other option for later)
        % Technically not necessary since they will be sorted if this
	% interface is used
        sort('=<',Terms,SortedTerms),
        get_data_from_terms(SortedTerms,Options,Data).



%%%%%%%
% get_data_from_terms(++Terms,++Options,--Data)
%%%%%%%


get_data_from_terms([],_,[]) :-
        !.

% Complete computation of data
get_data_from_terms([Term|Rest_Terms],Options,Data) :-
        not_member(range(_,_),Options),
        not_member(ranges(_),Options),
        !,
        Term =.. [_Functor|Parameters],
        (member(data_position(Pos),Options) ->
            nth1(Pos,Parameters,Sequence_Data)
        ;
            nth1(4,Parameters,Sequence_Data)  % Default position
        ),
        get_data_from_terms(Rest_Terms,Options,Rest_Data),
        append(Sequence_Data,Rest_Data,Data).


% Range Management
get_data_from_terms(Terms,Options,Data) :-
        member(range(Min,Max),Options),
        !,
        Sequence_Data = [],
        Ranges = [[Min,Max]],
        Current_Position = 1,
        Current_Data = [],
        %Term =.. [_Functor|Parameters],
        (member(data_position(Pos),Options) ->
            true
        ;
            Pos = 4
        ),
        range_info_position(Options,Range_Info_Position),
        (Range_Info_Position = 'none' ->
            get_data_from_terms_rec(Sequence_Data,Ranges,Terms,Current_Position,Current_Data,Pos,Data) % Could be dangerous called with one parameter removed Range_Info_Position
        ;
            get_data_from_terms_rec(Sequence_Data,Ranges,Terms,Current_Position,Current_Data,Pos,Range_Info_Position,Data)
        ).



% Ranges Management
get_data_from_terms(Terms,Options,Data) :-
        member(ranges(Ranges),Options),
        Sequence_Data = [],
        Current_Position = 1,
        Current_Data = [],
        (member(data_position(Pos),Options) ->
            true
        ;
            Pos = 4
        ),
        range_info_position(Options,Range_Info_Position),
        (Range_Info_Position = 'none' ->
            get_data_from_terms_rec(Sequence_Data,Ranges,Terms,Current_Position,Current_Data,Pos,Data) % Could be dangerous called with one parameter removed Range_Info_Position
        ;
            get_data_from_terms_rec(Sequence_Data,Ranges,Terms,Current_Position,Current_Data,Pos,Range_Info_Position,Data)
        ).




%%%%
% get_data_from_terms_rec(++Sequence_Data,++Ranges,++Terms,++Current_Position,++Current_Data,++Pos,++Range_Info_Position,--Data)
% Description: this predicated built the wanted data given all the parameters
%%%%

% Termination case: Sequence Data = [] and Terms = []
get_data_from_terms_rec([],_Ranges,[],_Current_Position,Current_Data,_Pos,_Range_Info_Position,[]) :-
        empty_lists(Current_Data),
        !.


% Termination case: Ranges = empty
get_data_from_terms_rec(_Sequence_Data,[],_Terms,_Current_Position,Current_Data,_Pos,_Range_Info_Position,[]) :-
        empty_lists(Current_Data),
        !.

% Case: Sequence data empty => we look for a new one 
get_data_from_terms_rec([],[[Min,Max]|Ranges],[Term|Rest_Terms],Current_Position,Current_Data,Pos,(_Left,Right),Data) :-
        !,
        Term =.. [_Functor|Parameters],
        nth1(Right,Parameters,RightPos),
        (RightPos < Min ->      % Test to skip the term
            Current_Position2 is RightPos+1,
            get_data_from_terms_rec([],[[Min,Max]|Ranges],Rest_Terms,Current_Position2,Current_Data,Pos,(_Left,Right),Data)
        ;
            Current_Position = Current_Position2,
            nth1(Pos,Parameters,Sequence_Data),
            get_data_from_terms_rec(Sequence_Data,[[Min,Max]|Ranges],Rest_Terms,Current_Position2,Current_Data,Pos,(_Left,Right),Data)
        ).



% Case: Current Position < Minimal bound of the first Range
get_data_from_terms_rec([_Val|Rest_Sequence],[[Min,Max]|Ranges],Terms,Current_Position,Current_Data,Pos,Range_Info_Position,Data) :-
       Current_Position < Min,
       !,
       Current_Position1 is Current_Position+1,
       get_data_from_terms_rec(Rest_Sequence,[[Min,Max]|Ranges],Terms,Current_Position1,Current_Data,Pos,Range_Info_Position,Data).



% Case: Minimal bound = Current_Position => Start Data building.
% Inv: Current_Data is empty as the first Range of Ranges has Min as minimal bound.
get_data_from_terms_rec([Val|Rest_Sequence],[[Min,Max]|Ranges],Terms,Current_Position,[],Pos,Range_Info_Position,[[Val|Rest_Range_Data]|Rest_Data]) :-
       Current_Position = Min,
       !,
       check_ranges_and_update_data(Val,Ranges,Current_Position,Rest_Range_Data,[],Current_Data_Update,Rest_Data,Rest_Data_Update),
       Current_Position1 is Current_Position+1,
       get_data_from_terms_rec(Rest_Sequence,[[Min,Max]|Ranges],Terms,Current_Position1,Current_Data_Update,Pos,Range_Info_Position,Rest_Data_Update).



% Case: Minimal bound =< Current_Position =< Maximal Bound: update Current Data range and look for overlap
get_data_from_terms_rec([Val|Rest_Sequence],[[Min,Max]|Ranges],Terms,Current_Position,Current_Data,Pos,Range_Info_Position,Data) :-
       Min < Current_Position,
       Current_Position < Max,
       !,
       Current_Position1 is Current_Position+1,
       Current_Data = [T|Rest_Current_Data],
       T = [Val|T_Rest],
       check_ranges_and_update_data(Val,Ranges,Current_Position,T_Rest,Rest_Current_Data,Current_Data_Update,Data,Data_Update),
       get_data_from_terms_rec(Rest_Sequence,[[Min,Max]|Ranges],Terms,Current_Position1,Current_Data_Update,Pos,Range_Info_Position,Data_Update).


% Case: Maximal bound = Current_Position
% End of Range data buiding
get_data_from_terms_rec([Val|Rest_Sequence],[[_Min,Max]|Ranges],Terms,Current_Position,Current_Data,Pos,Range_Info_Position,Data) :-
       Current_Position = Max,
       !,
       Current_Position1 is Current_Position+1,
       Current_Data = [T|Rest_Current_Data],
       T = [Val],
       check_ranges_and_update_data(Val,Ranges,Current_Position,[],Rest_Current_Data,Current_Data_Update,Data,Data_Update),
       get_data_from_terms_rec(Rest_Sequence,Ranges,Terms,Current_Position1,Current_Data_Update,Pos,Range_Info_Position,Data_Update).

% Case: Data Collection finished but Range not yet removed (overlap inside between Range1 Range2 = Min1 < Min2 but Max2 =< Max1)


get_data_from_terms_rec(Sequence_Data,[[_Min,Max]|Ranges],Terms,Current_Position,Current_Data,Pos,Range_Info_Position,Data) :-
       Current_Position > Max,
       !,
       Current_Data = [[]|Rest_Current_Data],
       get_data_from_terms_rec(Sequence_Data,Ranges,Terms,Current_Position,Rest_Current_Data,Pos,Range_Info_Position,Data).




%%%%
% get_data_from_terms_rec(++Sequence_Data,++Ranges,++Terms,++Current_Position,++Current_Data,++Pos,--Data)
% Description: same predicate than below but without jump thanks to left_position or right_position
%%%%


% Termination case: Sequence Data = [] and Terms = []
get_data_from_terms_rec([],_Ranges,[],_Current_Position,Current_Data,_Pos,[]) :-
        empty_lists(Current_Data),
        !.


% Termination case: Ranges = empty
get_data_from_terms_rec(_Sequence_Data,[],_Terms,_Current_Position,Current_Data,_Pos,[]) :-
        empty_lists(Current_Data),
        !.

% Case: Sequence data empty => we look for a new one 
get_data_from_terms_rec([],[[Min,Max]|Ranges],[Term|Rest_Terms],Current_Position,Current_Data,Pos,Data) :-
        !,
        Term =.. [_Functor|Parameters],
        nth1(Pos,Parameters,Sequence_Data),
        get_data_from_terms_rec(Sequence_Data,[[Min,Max]|Ranges],Rest_Terms,Current_Position,Current_Data,Pos,Data).



% Case: Current Position < Minimal bound of the first Range
get_data_from_terms_rec([_Val|Rest_Sequence],[[Min,Max]|Ranges],Terms,Current_Position,Current_Data,Pos,Data) :-
       Current_Position < Min,
       !,
       Current_Position1 is Current_Position+1,
       get_data_from_terms_rec(Rest_Sequence,[[Min,Max]|Ranges],Terms,Current_Position1,Current_Data,Pos,Data).



% Case: Minimal bound = Current_Position => Start Data building.
% Inv: Current_Data is empty as the first Range of Ranges has Min as minimal bound.
get_data_from_terms_rec([Val|Rest_Sequence],[[Min,Max]|Ranges],Terms,Current_Position,[],Pos,[[Val|Rest_Range_Data]|Rest_Data]) :-
       Current_Position = Min,
       !,
       Current_Data = [Rest_Range_Data],
       Current_Position1 is Current_Position+1,
       get_data_from_terms_rec(Rest_Sequence,[[Min,Max]|Ranges],Terms,Current_Position1,Current_Data,Pos,Rest_Data).



% Case: Minimal bound =< Current_Position =< Maximal Bound: update Current Data range and look for overlap
get_data_from_terms_rec([Val|Rest_Sequence],[[Min,Max]|Ranges],Terms,Current_Position,Current_Data,Pos,Data) :-
       Min < Current_Position,
       Current_Position < Max,
       !,
       Current_Position1 is Current_Position+1,
       Current_Data = [T|Rest_Current_Data],
       T = [Val|T_Rest],
       check_ranges_and_update_data(Val,Ranges,Current_Position,T_Rest,Rest_Current_Data,Current_Data_Update,Data,Data_Update),
       get_data_from_terms_rec(Rest_Sequence,[[Min,Max]|Ranges],Terms,Current_Position1,Current_Data_Update,Pos,Data_Update).


% Case: Maximal bound = Current_Position
% End of Range data buiding
get_data_from_terms_rec([Val|Rest_Sequence],[[_Min,Max]|Ranges],Terms,Current_Position,Current_Data,Pos,Data) :-
       Current_Position = Max,
       !,
       Current_Position1 is Current_Position+1,
       Current_Data = [T|Rest_Current_Data],
       T = [Val],
       check_ranges_and_update_data(Val,Ranges,Current_Position,[],Rest_Current_Data,Current_Data_Update,Data,Data_Update),
       get_data_from_terms_rec(Rest_Sequence,Ranges,Terms,Current_Position1,Current_Data_Update,Pos,Data_Update).

% Case: Data Collection finished but Range not yet removed (overlap inside between Range1 Range2 = Min1 < Min2 but Max2 =< Max1)


get_data_from_terms_rec(Sequence_Data,[[_Min,Max]|Ranges],Terms,Current_Position,Current_Data,Pos,Data) :-
       Current_Position > Max,
       !,
       Current_Data = [[]|Rest_Current_Data],
       get_data_from_terms_rec(Sequence_Data,Ranges,Terms,Current_Position,Rest_Current_Data,Pos,Data).

%%%
% utils get_data_from_terms_rec
%%%

empty_lists([]) :-
        !.

empty_lists([A|Rest]) :-
        A = [],
       empty_lists(Rest).


	% check_ranges_and_update_data(++Val,++Ranges,++Current_Position,++T_Rest,++Current_Data,--Current_Data_Update,++Data,--Data_Update)

	% Case: no more range, data range under construction
	check_ranges_and_update_data(_Val,[],_Current_Position,T_Rest,_Current_Data,Current_Data_Update,Data,Data) :-
	        var(T_Rest),
	        !,
	        Current_Data_Update = [T_Rest].

	% Case: no more range, data range under construction finished
	check_ranges_and_update_data(_Val,[],_Current_Position,[],_Current_Data,Current_Data_Update,Data,Data) :-
	        !,
	        Current_Data_Update = [].

	% Case: Ranges avalaible, Data range under construction
	check_ranges_and_update_data(Val,Ranges,Current_Position,T_Rest,Current_Data,[T_Rest|Current_Data_Update],Data,Data_Update) :-
	        var(T_Rest),
	        !,
	        check_ranges_and_update_data_rec(Val,Ranges,Current_Position,Current_Data,Current_Data_Update,Data,Data_Update).

	% Case: Ranges avalaible, Data range finished
	check_ranges_and_update_data(Val,Ranges,Current_Position,[],Current_Data,Current_Data_Update,Data,Data_Update) :-
	        !,
	        check_ranges_and_update_data_rec(Val,Ranges,Current_Position,Current_Data,Current_Data_Update,Data,Data_Update).


% Recurisve call
% No more Range = end iteration
check_ranges_and_update_data_rec(_Val,[],_Current_Position,[],[],Data,Data).

% Case: Current Data empty + no new start of data
check_ranges_and_update_data_rec(_Val,[[Min,_Max]|_Rest_Ranges],Current_Position,[],[],Data,Data) :-
        Current_Position < Min,
        !.


% Case: Current Data empty + new start of data range
check_ranges_and_update_data_rec(Val,[[Min,_Max]|Rest_Ranges],Current_Position,[],[Rest_Range|Rest_Data_Update],[[Val|Rest_Range]|Rest_Data],Data_Update) :-
        Current_Position = Min,
        !,
        check_ranges_and_update_data_rec(Val,Rest_Ranges,Current_Position,[],Rest_Data_Update,Rest_Data,Data_Update).


% Case: Current Data not empty + Current Position in Min Max => Update
check_ranges_and_update_data_rec(Val,[[Min,Max]|Rest_Ranges],Current_Position,[Range_Data|Rest_Current_Data],Current_Data_Update,Data,Data_Update) :-
        Min < Current_Position,
        Current_Position =< Max,
        !,
        Range_Data = [Val|Range_Data2],
        Current_Data_Update = [Range_Data2|Current_Data_Update2],
        check_ranges_and_update_data_rec(Val,Rest_Ranges,Current_Position,Rest_Current_Data,Current_Data_Update2,Data,Data_Update).

% Case: overlap, Range data terminated and look for an other overlap
check_ranges_and_update_data_rec(Val,[[_Min,Max]|Rest_Ranges],Current_Position,[Range_Data|Rest_Current_Data],Current_Data_Update,Data,Data_Update) :-
        Current_Position > Max,
        !,
        Current_Data_Update = [Range_Data|Current_Data_Update2],
        check_ranges_and_update_data_rec(Val,Rest_Ranges,Current_Position,Rest_Current_Data,Current_Data_Update2,Data,Data_Update).

% range_info_position(++Options,--Range_info_Position)

range_info_position(Options,Range_Position) :-
        member(left_position(Left),Options),
        Left = 'none',
        !,
        Range_Position = 'none'.

range_info_position(Options,Range_Position) :-
        member(right_position(Right),Options),
        Right = 'none',
        !,
        Range_Position = 'none'.

range_info_position(Options,Range_Position) :-
        member(left_position(Left),Options),
        member(right_position(Right),Options),
        Left \= 'none',
        Right \= 'none',
        !,
        Range_Position = (Left,Right).


range_info_position(Options,Range_Position) :-
        not_member(left_position(_Left),Options),
        not_member(right_position(_Right),Options),
        !,
        Range_Position = (2,3).

range_info_position(Options,Range_Position) :-
        not_member(left_position(_Left),Options),
        member(right_position(Right),Options),
        Right \= 'none',
        !,
        Range_Position = (2,Right).

range_info_position(Options,Range_Position) :-
        member(left_position(Left),Options),
        not_member(right_position(_Right),Options),
        Left \= 'none',
        !,
        Range_Position = (Left,3).





seqdb_annotation_from_file(Options,File,Annotation) :-
        (\+ member(consult)),
        !,
        terms_from_file(File,Terms),
        % Technically not necessary since they will be sorted if this
		% interface is used
		sort('=<',Terms,SortedTerms), 
        sequence_terms_to_annotations(Options,SortedTerms,Annotation).

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



%--------------------------------
% Saving information to file  %
%--------------------------------

%% sequence_to_file(+KeyIndex,+ChunkSize,+Sequence,+File)
% 
% Stores List to File in the =|text(prolog(sequence(_)))|= format, e.g. Prolog facts on the form:
% ==
% data(KeyIndex,1,5, [a,t,c,c,g]).
% ==
% The first argument KeyIndex is used as an identifier. 
% ChunkSize is the number of elements from Sequence to store with each fact, e.g. it is 5 in the example above.
% The second and third argument of a stored fact, corresponds to the start and end position in Sequence (indexing starts at position one).
% The fourth argument of the fact is a list containing the relevant elements of the Sequence list. 
% Note that if the the length of Sequence is not a multiple of ChunkSize, then the last fact stored will have a shorter range.
sequence_to_file(KeyIndex,ChunkSize,Annotation,File) :-
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





