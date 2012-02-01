:- module(annotation_index,[lost_file_index_get_filename/6]).
/** <module> annotation index 

The annotation index is used to keep track of files generated by running tasks in models.
It is primarily used internally, and is not expected te be used directly from models.
In fact, updating the index from a model (which runs a separate process), may damage the index.

The index file, $LOST_BASE_DIR/annotation.idx contains a list of facts on the form

==
fileid(FileId,Filename,Model,Goal,InputFiles,Options)
==

where InputFiles and Options are a lists.

The quadruple (Model,Goal,InputFiles,Options) uniquely identifies Filename and the relation is
deterministic in this sense.

@author: Christian Theil Have

*/

:- use(arithmetic).
:- use(lists).

%% lost_file_index_get_filenames(+IndexFile,+Model,+Goal,+InputFiles,+Options,-Filenames) is det
% Retrieve the filename matching (Model,Options,InputFiles) from the file index
% If no such filename exists in the index, then a new unique filename is created
% and unified to Filename.
lost_file_index_get_filenames(IndexFile,Model,Goal,InputFiles,Options,Filenames) :-
	(file_exists(IndexFile) -> terms_from_file(IndexFile,Terms) ; Terms = []),
	(lost_file_index_get_filenames_from_terms(Terms,Model,Goal,InputFiles,Options,Filenames) ->
	 true
	;
	 lost_file_index_timestamp(Ts),
	 term2atom(Ts,AtomTs),
	 forall(member(F,Filenames), lost_create_and_reserve_file(IndexFile,F)),
	 append(Terms, [fileid(IndexAtom,AtomTs,Filenames,Model,Goal,InputFiles,Options)],NewTerms),
	 terms_to_file(IndexFile,NewTerms)
	).

lost_create_and_reserve_file(IndexFile,F) :-
	 lost_file_index_next_available_index(Terms, Index),
	 dirname(IndexFile,IndexDir),
	 term2atom(Index,IndexAtom),
	 atom_concat_list([IndexDir, Model, '_',Goal,'_',IndexAtom,'.gen'], Filename).


%% lost_file_index_next_available_index(+Terms, -NextAvailableIndex)
% Given Terms, unify NextAvailableIndex with a unique index not occuring as index in terms.
lost_file_index_next_available_index(Terms, NextAvailableIndex) :-
	lost_file_index_largest_index(Terms,LargestIndex),
	NextAvailableIndex is LargestIndex + 1.

%% lost_file_index_largest_index(+Terms, 0).
% Unify second argument with largest index occuring in terms
lost_file_index_largest_index([], 0).
lost_file_index_largest_index([Term|Rest], LargestIndex) :-
	Term =.. [ fileid, TermIndexAtom | _ ],
	atom_integer(TermIndexAtom,Index),
	lost_file_index_largest_index(Rest,MaxRestIndex),
	max(Index,MaxRestIndex,LargestIndex).

%% lost_file_index_get_filenames_from_terms/5:
% Go through a list terms and check find a Filename matching (Model,ParamsId,InputFiles)
% Fail if no such term exist
lost_file_index_get_filenames_from_terms([Term|_],Model,Goal,InputFiles,Options,Filenames) :-
	Term =.. [ fileid, _, _, Filenames, Model, Goal, InputFiles, Options ],
	!.

lost_file_index_get_filenames_from_terms([_|Rest],Model,Goal,InputFiles,Options,Filenames) :-
	lost_file_index_get_filenames_from_terms(Rest,Model,Goal,InputFiles,Options,Filenames).

%% lost_file_index_timestamp(time(-Year,-Mon,-Day,-Hour,-Min,-Sec))
% Get a timestamp corresponding to the current time
lost_file_index_timestamp(time(Year,Mon,Day,Hour,Min,Sec)) :-
	date(Year,Mon,Day),
	time(Hour,Min,Sec).

%% lost_file_index_get_file_timestamp(+IndexFile,+Filename,-Timestamp) is det
% Timestamp is a term 
% ==
% time(Year,Mon,Day,Hour,Min,Sec)
% ==
% which symbolize the time at which the file Filename was generated.
lost_file_index_get_file_timestamp(IndexFile,Filename,Timestamp) :-
	terms_from_file(IndexFile,Terms),
	TermMatcher =.. [ fileid,  _Index, TimeStampAtom, Filenames, _Model, _Goal, _InputFiles, _Options ],
	member(TermMatcher,Terms),
	member(Filename,Filenames),
	parse_atom(TimeStampAtom,Timestamp).
				   
%% lost_file_index_update_file_timestamp(+IndexFile,+Filename)
% Update the timestamp associated with Filename to a current timestamp.
% This should be used if the file is (re) generated.
lost_file_index_update_file_timestamp(IndexFile,Filename) :-
	lost_file_index_timestamp(Ts),
	term2atom(Ts,TsAtom),
	terms_from_file(IndexFile,Terms),
	OldTermMatcher =.. [ fileid,  Index, _, Filenames, Model, Goal, InputFiles, Options ],
	member(OldTermMatcher,Terms),
	member(Filename,Filenames),
	subtract(Terms,[OldTermMatcher],TermsMinusOld),
	NewTerm =.. [ fileid,  Index, TsAtom, Filenames, Model, Goal, InputFiles, Options ],
	append(TermsMinusOld,[NewTerm],UpdatedTerms),
	terms_to_file(IndexFile,UpdatedTerms).

%% lost_file_index_filename_member(+IndexFile,?Filename)
% True if Filename occurs in the annotation index identified by IndexFile.
lost_file_index_filename_member(IndexFile, Filename) :-
	terms_from_file(IndexFile,Terms),
	TermMatcher =.. [ fileid,  _Index, _Ts, Filenames, _Goal, _Model, _InputFiles, _Options ],
	member(TermMatcher,Terms),
	member(Filename, Filenames).

%% lost_file_index_inputfiles(+IndexFile,?Filename,?InputFiles)
% True if Filename occurs together with InputFiles in the annotation index identified by IndexFile.
lost_file_index_inputfiles(IndexFile,Filename,InputFiles) :-
	terms_from_file(IndexFile,Terms),
	TermMatcher =.. [ fileid,  _Index, _Ts, Filenames, _Goal,_Model, InputFiles, _Options ],
	member(TermMatcher,Terms),
	member(Filename,Filenames).

%% lost_file_index_move_file(+IndexFile,+OldFilename,+NewFilename) is det
% Allows renaming a file OldFilename occuring the annotation index (IndexFile) to a new name, NewFilename
% all references to the OldFilename in the IndexFile will be replaced by NewFilename.
lost_file_index_move_file(IndexFile,OldFilename,NewFilename) :-
	terms_from_file(IndexFile,Terms),
	OldTermMatcher =.. [ fileid,  Index, TS, OldFilenames, Model, Goal, InputFiles, Options ],
	subtract(Terms,[OldTermMatcher],TermsMinusOld),
	member(OldFilename,OldFilenames),
	replace(OldFilename,NewFilename,OldFilenames,NewFilenames),
	NewTerm =.. [ fileid,  Index, TS, NewFilenames, Model, Goal, InputFiles, Options ],
	append(TermsMinusOld,[NewTerm],UpdatedTerms),
	terms_to_file(IndexFile,UpdatedTerms).