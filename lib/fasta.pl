%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This file includes some utilities for working with the FASTA format
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- lost_include_api(misc_utils).

%
% fasta_load_sequence(++InputFile,++SequenceIdentifier,--FastaHeaderLine,--Sequence)
% 
fasta_load_sequence(InputFile,SequenceIdentifier,FastaHeaderLine,Sequence) :-
	readFile(InputFile,FileContents),
	fasta_format(_, FastaEntries, FileContents, []),
	nth1(SequenceIdentifier, FastaEntries, [FastaHeaderLine,SequenceCodes]),
	map(upper_lower, SequenceCodes, SequenceCodesLowerCase),
	fasta_make_atom_list(SequenceCodesLowerCase, Sequence).

fasta_make_atom_list([], []).
fasta_make_atom_list([Code|CRest], [Atom|ARest]) :-
	atom_codes(Atom,[Code]),
	fasta_make_atom_list(CRest,ARest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DCG parser for the generic FASTA format
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fasta_format(EolType,FastaEntries) -->
	maybe_empty_lines,
	fasta_entries(EolType,FastaEntries).

fasta_entries(EolType,[FastaEntry|Rest]) --> fasta_entry(EolType,FastaEntry), fasta_entries(EolType,Rest).

fasta_entries(EolType,[FastaEntry]) --> fasta_entry(EolType,FastaEntry).

fasta_entry(EolType,[Header,Sequence]) -->
	fasta_header_line(EolType,Header),
	fasta_sequence(EolType,Sequence),
	maybe_empty_lines.

fasta_header_line(EolType,Header) -->
	[ 62 ], % code for '>'
	non_end_of_lines(Header),
	end_of_line(EolType).

fasta_sequence(EolType,[E|Rest]) -->
	fasta_sequence_entry(E),
	fasta_sequence(EolType,Rest).

fasta_sequence(EolType,Rest) -->
	end_of_line(EolType),
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