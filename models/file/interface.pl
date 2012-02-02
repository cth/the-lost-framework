:- ['../../lost.pl'].

:- use(lists).
:- use(prologdb).
	
:- task(get([url(Filetype)],[],Filetype)). % Guarantees nothing on which kind of files it retrieves.

:- task(from_terms([], [terms([])], text(prolog(_)))).

:- task(from_sequence([],[sequence([])], text(prolog(sequence(_))))).

%% get(+InputFiles, +Options, +OutputFile)
% ==
% InputFiles = [ URL ]
% ==
% Retrieves a file, either local or remote pointed to by URL.
% URL is an atom containing an URL specification, e.g. 'file://...' or 'http://...' etc.
% Note that, the =|OutputFile|= may be of any format.

% Getting via http or ftp
get([TargetURL],_Options,OutputFile) :-
	atom_codes(TargetURL,TargetSyms),
	map(atom_codes,['ftp://','http://', '"ftp://', '"http://'],Matchers),
	member(MatchSyms,Matchers),
	append(MatchSyms,_,TargetSyms),
	wget(TargetURL,OutputFile).
	
% Getting a file from the local file system
get([FileURL],Options,OutputFile) :-
	atom(File),
	atom_codes(FileURL,FileURLSyms),
	atom_codes('file://', MatchSyms),
	append(MatchSyms,FileCodes,FileURLSyms),
	atom_codes(File,FileCodes),
	get([File],Options,OutputFile).
	
get([File],_Options,OutputFile) :-
	file_exists(File),
	copy_file(File,OutputFile).
	
%% from_terms(+InputFiles,+Options,+OutputFile)
% =|InputFiles|= are ignored. The task creates a new file containing all the terms given by the option =|terms|=.
from_terms(_,Options,OutputFile) :-
	get_option(Options,terms,Terms),
	terms_to_file(OutputFile,Terms).	
	
%% from_sequence(+InputFiles,+Options,+OutputFile)
% =|InputFiles|= is ignored. This task creates a file from the sequence given with the option =|sequence|=.
from_sequence(_,Options,OutputFile) :-
	get_option(Options,sequence,Sequence),
	length(Sequence,SeqLength),
	terms_to_file(OutputFile,[sequence('n/a',1,SeqLength,Sequence)]).

% utility predicate for fetching a file via http	
wget(URL,OutputFile) :-
	atom_concat_list([wget, ' "', URL, '" --output-document=', OutputFile],Command),
	system(Command).
	
test :-
	get(['ftp://ftp.ncbi.nih.gov/genomes/Bacteria/Escherichia_coli_K_12_substr__MG1655_uid57779/NC_000913.Prodigal-2.50'],[],'test.out').
