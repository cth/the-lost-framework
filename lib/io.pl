/** <module> Module of Input/Output manipulation
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
%  09/03: creation of file                                   MP
%  26/04: modification of the load_annotation_from_file
%         predicate name options modfied. Old version kept   MP
% REMARKS: any problem, contact {cth,otl,petit}@(without advertissement)ruc.dk
%
% MODULS USED: misc_utils.pl
%
% NOTE TO THE USER : 
*/

:- lost_include_api(utils_parser_report).
:- lost_include_api(misc_utils).


%% load_annotation_from_file(+Type_Info,+Options,+File,-Annotation)
%
% Description: given some Options, generate an Annotation from a set of terms contained into File
%
%
% Type_Info: sequence
% Terms in File are composed of List of data.
% Annotation is a list
% Options available: Options = [data_position(Position),
%                               all_lists,range(Min,Max),
%                               range(Range)
%                               consult]
% all_lists does not support a range option
%
% Type: db
% Terms in File are composed of Range that delimites a specific region.
% Options available: Options = [in_db(Letter),not_in_db(Letter),
%                               range_position(Position),range(Min,Max)]
% Note: assumption is done on the format of the database. Information is extracted from a list
% of terms that have a parameter Range to describe a specific region of the genome.
% By defaut, annotation of the specific region is 1 and 0 when the region is not specific.
%



% Type sequence
%%%%%%%%%
load_annotation_from_file(sequence,Options,File,Annotation) :-
        (\+ member(consult)),
        !,
        terms_from_file(File,Terms),
        % Technically not necessary since they will be sorted if this
	% interface is used
	sort('=<',Terms,SortedTerms), 
        sequence_terms_to_annotations(Options,SortedTerms,Annotation).





% Type db
%%%%%%%
load_annotation_from_file(db,Options,File,Annotation) :-
        member(term(Terms),Options),
        !,
        (var(Terms) ->
            terms_from_file(File,Terms),
            sort('=<',Terms,SortedTerms)
        ;        
            SortedTerms = Terms
        ),
                                % Technically not necessary since they will be sorted if this
                                % interface is used
        db_terms_to_annotations(Options,SortedTerms,Annotation).

% TO FI
load_annotation_from_file(db,Options,File,Annotation) :-
        terms_from_file(File,Terms),
        sort('=<',Terms,SortedTerms),
        % Technically not necessary since they will be sorted if this
	% interface is used

        db_terms_to_annotations(Options,SortedTerms,Annotation).


% Utils for load_annotation_from_file(sequence,....)



% Utils for load_annotation_from_file(db)

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


% Inside the specified range AND inside a specific region
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Loading a sequence into memory
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load_sequence(SeqId, Filename) :-
	terms_from_file(Filename,Terms),
	Terms = [ FirstTerm | _ ],
	FirstTerm =.. [_functor,_id,Begin,End,_],
	BlockLen is (End - Begin) + 1,
	length(Terms,NumBlocks),
	assert(sequence_block_length(SeqId,BlockLen)),
	assert(sequence_blocks(SeqId,NumBlocks)),
	assert_sequence_terms(SeqId,Terms,0,NumBlocks).

unload_sequence(SeqId) :-
	retractall(sequence_block_length(SeqId,_)),
	retractall(sequence_blocks(SeqId,_)),
	retractall(sequence(SeqId,_,_)).

assert_sequence_terms(_,[],NumBlocks,NumBlocks).
assert_sequence_terms(Id, [Term|TRest], BlockNo, NumBlocks) :-
	assert(sequence(Id,BlockNo,Term)),
	NextBlockNo is BlockNo + 1,
	assert_sequence_terms(Id,TRest,NextBlockNo,NumBlocks).

get_sequence_range(SeqId, Min, Max, Data) :-
	sequence_block_length(SeqId,BlockLen),
	% The smallest block number in this sequence range:
	MinBlockNumber is (Min-1) // BlockLen,
	% The position at which the smallest block in this sequence range starts
	MinBlockStart is MinBlockNumber*BlockLen+1,
	% The relative position of Min in the first block 
	StartInBlock is Min-MinBlockStart + 1,
	% The number of block that includes Max:
	MaxBlockNumber is (Max-1) // BlockLen,
	((MinBlockNumber == MaxBlockNumber) -> % Min and Max are in same block
	 EndInBlock is Max-MinBlockStart + 1,
	 get_block_part(SeqId,MinBlockNumber,StartInBlock,EndInBlock,Data)
	;
	 get_block_part(SeqId,MinBlockNumber,StartInBlock,BlockLen,Part),
	 NextMin is Min + (BlockLen-StartInBlock) + 1,
	 get_sequence_range(SeqId,NextMin,Max,PartsRest),
	 append(Part,PartsRest,Data)
	).

get_block_part(SeqId,BlockNumber,StartInBlock, EndInBlock, PartData) :-
	sequence(SeqId,BlockNumber,Term),
	Term =.. [ _functor, _id, _start, _end, BlockData ],
	get_block_part_rec(BlockData,1,StartInBlock,EndInBlock,PartData).

get_block_part_rec([DataItem|_],Pos,_, EndInBlock, [DataItem]) :-
	Pos == EndInBlock.

get_block_part_rec([_|BlockData],Pos,StartInBlock, EndInBlock, PartData) :-
	Pos < StartInBlock,
	NextPos is Pos + 1,
	get_block_part_rec(BlockData,NextPos,StartInBlock,EndInBlock,PartData).

get_block_part_rec([DataItem|BlockData],Pos,StartInBlock, EndInBlock, [DataItem|PartData]) :-
	Pos >= StartInBlock,
	NextPos is Pos + 1,
	get_block_part_rec(BlockData,NextPos,StartInBlock,EndInBlock,PartData).


% A CP approach to do that (small test) not finished, data file should be consulted.
%test(Nb_Nuc,Range_Min,Range_Max,Result) :-
%        lost_sequence_file('U00096',Sequence_File),
%        open(Sequence_File,read,Stream),
%        findall([Min,Max,Data],(data(_Key,Min,Nax,Data),)Min+Nb_Nuc #>=Range_Min,Min #=< Range_Max,Range_Min#=<Max,Max-Nb_Nuc#=<Range_Max),Result).








        

