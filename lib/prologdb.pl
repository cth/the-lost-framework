:- module(prologdb,[file_functor/2,file_functors/2,terms_from_file/2, terms_to_file/2, split_prolog_file/4,merge_prolog_files/2]).
/** <module> accessing Prolog source files.

Predicates for working with Prolog source files, i.e. in the format =|text(prolog(_any))|=.

@author Christian Theil Have

*/

%% file_functor(+File,?Functor)
% True if all terms in File have the functor Functor.
file_functor(Filename, Functor) :-
	file_functors(Filename,[Functor]).
	
%% file_functors(+File,?Functors) is det
% Functors is a set of the functors in File
file_functors(Filename, SetFunctors) :-
	terms_from_file(Filename,Terms),
	findall( F,((member(X,Terms), X =.. [ F |_ ])), AllFunctors),
	eliminate_duplicate(AllFunctors,SetFunctors).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Basic reading/writing of terms to/from file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% terms_from_file(+File,-Terms)
% Reads all Terms from named file File
terms_from_file(File, Terms) :-
	open(File, read, Stream),
	ground(Stream),
	collect_stream_terms(Stream,Terms),
	close(Stream).

% terms_to_file(+File,+Terms)
% Writes all Terms to named file File
terms_to_file(File,Terms) :-
	open(File,write,Stream),
	ground(Stream),
	write_terms_to_stream(Stream,Terms),
	close(Stream).

% write_terms_to_stream(+Stream:stream, +Terms:list)
% Writes Terms to a Stream
write_terms_to_stream(_,[]).
write_terms_to_stream(Stream,[Term|Rest]) :-
	writeq(Stream,Term),
	write(Stream,'.\n'),
	write_terms_to_stream(Stream,Rest).

%% collect_stream_terms(+Stream,-Terms)
% Terms is a list of all Terms read from Stream untill end_of_file
collect_stream_terms(Stream, Rules) :- 
	read(Stream, T),
	((T == end_of_file) ->
	 Rules = []
	;
	 collect_stream_terms(Stream,Rest),
	 append([T],Rest,Rules)
	).



%% split_prolog_file(+File,+ChunkSize,+OutputFilePrefix,+OutputFileSuffix)
%
% Split a file of Prolog terms into multiple files, such that each file contains
% a (disjunct) fraction of the Prolog terms in File. Each of the resulting files
% are valid Prolog files. 
% 
% - File is the file to split up. 
% - ChunkSize is the maximal number of terms in resulting fragment files. 
% - OutputFilePrefix is a prefix given to resulting fragment files
% - OutputFileSuffix is a suffix given to resulting fragment files
split_prolog_file(Filename,ChunkSize,OutputFilePrefix, OutputFileSuffix) :-
        split_file(Filename,ChunkSize,OutputFilePrefix, OutputFileSuffix,_).
split_prolog_file(Filename,ChunkSize,OutputFilePrefix, OutputFileSuffix,ResultingFiles) :-
       open(Filename,read,Stream),
       split_file_loop(Stream,ChunkSize,1,OutputFilePrefix, OutputFileSuffix,ResultingFiles),
       close(Stream).

split_prolog_file_loop(IStream, ChunkSize, FileNo, OutputFilePrefix,OutputFileSuffix,ResultingFiles) :-
	atom_integer(FileNoAtom,FileNo),
	atom_concat_list([OutputFilePrefix,'_',FileNoAtom,OutputFileSuffix], OutputFile),
	write('creating split file:'), write(OutputFile),nl,
	read_next_n_terms(ChunkSize,IStream,Terms),
	((Terms == []) ->
         ResultingFiles = []
	;
	 terms_to_file(OutputFile,Terms),
	 NextFileNo is FileNo + 1,
	 length(Terms,NumTerms),
	 ((NumTerms < ChunkSize) ->									% if so, don't scan further
          ResultingFiles = [OutputFile]
	 ;																					% else, go again
	        ResultingFiles = [OutputFile|RestResultingFiles],
  			split_prolog_file_loop(IStream,ChunkSize,NextFileNo,OutputFilePrefix,OutputFileSuffix,RestResultingFiles)
	 )	 
	).

% Utils split_file
read_next_n_terms(0,_,[]).
read_next_n_terms(N,Stream,Terms) :-
	read(Stream,Term),
    ((Term == end_of_file) ->
	 Terms = []
	;
	 Terms = [Term|RestTerms],
	 !,
	 N1 is N - 1,
	 read_next_n_terms(N1,Stream,RestTerms)
	).


%% merge_prolog_files(+SeparateFiles,+MergedFile) is det
%
% Merges terms from SeparateFiles into the file MergedFile
merge_prolog_files(SmallFiles_List, BiggerFile_Name):-
	open(BiggerFile_Name,write, OutStream),
	merge_prolog_files_rec(SmallFiles_List,OutStream),
	close(OutStream).

merge_prolog_files_rec([],_OutStream):-!.
merge_prolog_files_rec([File|Files],OutStream):-
	terms_from_file(File,Terms),
	write_terms_to_stream(OutStream,Terms),
	writeln(File),
	writeln(Files), % (Prolog bug !!!) Files seems to interpreted as an atom rather than a list here ??? 
	merge_prolog_files_rec(Files,OutStream).