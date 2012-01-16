:- module(fasta,[fasta_load_sequence/4, fasta_save_sequence/3, split_file_fasta/5]).

/** <module> working with the FASTA file-format

This file includes some utilities for working with the FASTA format

@author: Christian Theil Have
@author: Matthieu Petit
@author: Ole Torp Lassen

*/

:- lost_include_api(misc_utils).
:- lost_include_api(io).
:- use(sequencedb).

%% fasta_load_sequence(+InputFile,+SequenceIdentifier,-FastaHeaderLine,-Sequence)
%
% From a FASTA file, this predicate transforms the header and data lines in
% in a Prolog format
fasta_load_sequence(InputFile,SequenceIdentifier,FastaHeaderLine,Sequence) :-
	readFile(InputFile,FileContents),
	write('after read file'),nl,
	fasta_format(_, FastaEntries, FileContents, []),
	write('done parsing fasta filee'),nl,
	nth1(SequenceIdentifier, FastaEntries, [FastaHeaderLine,SequenceCodes]),
	map(upper_lower, SequenceCodes, SequenceCodesLowerCase),
	atom_code_list(SequenceCodesLowerCase, Sequence).

%% fasta_save_sequence(+OutputFile,+SequenceData,+Header)
%
% Given a header and a sequence of data, this predicate
% writes in Outputfile SequenceData and Header in the fasta
% format.

fasta_save_sequence(OutputFile,SequenceData,Header) :-
	open(OutputFile,write,OS),
	write(OS,'>'),
	write(OS,Header),
	write(OS,'\n'),
	atom_code_list(SequenceCodes, SequenceData), 	% Convert sequence to codes
	map(upper_lower(output,input), SequenceCodes, SequenceCodesUppercase), % Convert all codes to upper case
	split_list_in_chunks(70,SequenceCodesUppercase,Chunks), % Divide the sequence into chunks for each fasta line
	map(atom_codes(output,input),Chunks,AtomChunks), % Convert each chunk to an atom
	forall(member(X,AtomChunks),(write(OS,X),write(OS,'\n'))), % write all lines
	close(OS).

	
atom_code_list([], []).
atom_code_list([Code|CRest], [Atom|ARest]) :-
	atom_codes(Atom,[Code]),
	atom_code_list(CRest,ARest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DCG parser for the generic FASTA format
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fasta_format(EolType,FastaEntries) -->
	{assert(line_count(1)),!},
	maybe_empty_lines,
	fasta_entries(EolType,FastaEntries).

fasta_entries(EolType,[FastaEntry|Rest]) -->
	fasta_entry(EolType,FastaEntry),
	fasta_entries(EolType,Rest).

fasta_entries(EolType,[FastaEntry]) --> fasta_entry(EolType,FastaEntry).

fasta_entry(EolType,[Header,Sequence]) -->
	fasta_header_line(EolType,Header),
	{!},
	fasta_sequence(EolType,Sequence),
	{!},
	maybe_empty_lines.

fasta_header_line(EolType,Header) -->
	[ 62 ], % code for '>'
	non_end_of_lines(Header),
	end_of_line(EolType).

fasta_sequence(EolType,[E|Rest]) -->
	fasta_sequence_entry(E),
	{!},
	fasta_sequence(EolType,Rest).

fasta_sequence(EolType,Rest) -->
	end_of_line(EolType),
	{
	 line_count(C1),
	 retractall(line_count(_)),
	 C2 is C1 + 1,
	 assert(line_count(C2)),
	 write('parsed line '),
	 write(C1),
	 nl
	},
	fasta_sequence(EolType,Rest).

fasta_sequence(EolType,[]) -->
	end_of_line(EolType).
	
fasta_sequence_entry(E) -->
	[ E ],
	{ not(member(E,[62,10,13])) }.


maybe_empty_lines -->
	space,
	maybe_empty_lines.
maybe_empty_lines -->
	end_of_line(_),
	maybe_empty_lines.
maybe_empty_lines --> [].

non_end_of_lines([Code|Rest]) --> non_end_of_line(Code), non_end_of_lines(Rest).
non_end_of_lines([Code]) --> non_end_of_line(Code).
		     
non_end_of_line(Code) --> [ Code ], { not(member(Code, [10,13])) }.

end_of_line(windows)--> [10,13].			% windows end of line
end_of_line(unix) --> [10].    % unix end of line

space --> [ 9 ]. % tab character
space --> [ 32 ]. % normal space character


%% split_file_fasta(+Filename,+ChunkSize,+OutputFilePrefix,+OutputFileSuffix,-ResultFiles)
%
% Split a FASTA composed of several header (> ...) into multiple files. We consider that a chunk
% has been seen each time that the symbol > appears at the beginning of a line
split_file_fasta(_Filename,ChunkSize,_OutputFilePrefix,_OutputFileSuffix,[]) :-
        ChunkSize =< 0,
        !,
        write("ChunkSize should be a non-negative number"),
        nl.

split_file_fasta(Filename,ChunkSize,OutputFilePrefix,OutputFileSuffix,ResultingFiles) :-
        open(Filename,read,IStream),
        readline(IStream,Firsline),
        split_file_fasta_rec(IStream,ChunkSize,1,OutputFilePrefix,OutputFileSuffix,Firsline,ResultingFiles),
        close(IStream).
		


split_file_fasta_rec(IStream, ChunkSize, FileNo, OutputFilePrefix,OutputFileSuffix,Firstline,ResultingFiles) :-
        number_codes(FileNo,Code),
        atom_codes(FileNo_Atom,Code),
        lost_tmp_directory(Tmp),
	atom_concat_list([Tmp,OutputFilePrefix,'_',FileNo_Atom,'.',OutputFileSuffix], OutputFile),
	write('creating split file:'), write(OutputFile),nl,
        open(OutputFile,write,OStream),
	read_next_n_chunk(Firstline,ChunkSize,IStream,OStream,EOF,LastLine),
        close(OStream),
	((EOF == yes) ->
            true,
            ResultingFiles = [OutputFile]
	;
            ResultingFiles = [OutputFile|RestResultingFiles],
            NextFileNo is FileNo+1,
            split_file_fasta_rec(IStream,ChunkSize,NextFileNo,OutputFilePrefix,OutputFileSuffix,LastLine,RestResultingFiles)
	).


% Read and write N chunk in FASTA format

% EOF reachs
read_next_n_chunk([-1],ChunkSize,_IStream,_OStream,yes,[-1]) :-
        ChunkSize >=0,
        !.

% Case: Line starts with > and N chunk already read
read_next_n_chunk([62|Rest],0,_IStream,_OStream,no,[62|Rest]) :-
        !.

% Case: Line starts with > and less than N chunk has been already read
read_next_n_chunk([62|Rest],ChunkSize,IStream,OStream,EOF,LastLine) :-
        ChunkSize > 0,
        !,
        atom_codes(Atom,[62|Rest]),
        write(OStream,Atom),
        nl(OStream),
        readline(IStream,CodeList),
        ChunkSize1 is ChunkSize-1,
        read_next_n_chunk(CodeList,ChunkSize1,IStream,OStream,EOF,LastLine).

% Default Case
read_next_n_chunk(CodeList,ChunkSize,IStream,OStream,EOF,LastLine) :-
        ChunkSize >= 0,
        !,
        atom_codes(Atom,CodeList),
        write(OStream,Atom),
        nl(OStream),
        readline(IStream,New_CodeList),
        read_next_n_chunk(New_CodeList,ChunkSize,IStream,OStream,EOF,LastLine).



