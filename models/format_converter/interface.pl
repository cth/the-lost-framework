%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Converts between different commonly used file-formats
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ['../../lost.pl'].
:- lost_include_api(interface).
:- lost_include_api(fasta).
:- lost_include_api(io).

lost_option(annotate,input_format,text(_),'Used to specify the format of the input file.').
lost_option(annotate,output_format,text(_),'Used to specify the format of the output file.').
lost_option(annotate,sequence_index, 1, 'Used to specify the index of a particular sequence in a multisequence format.').
	    
% Some ideas for specification of input & output formats...
lost_input_formats(annotate, [text(_)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lost_output_format/3 declarations
% Specify an output format predicate for each input_format, output_format pair
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% fasta (single sequence)  ==> text(prolog(sequence(_)))
lost_output_format(annotate, Options, text(prolog(range(SequenceType)))) :-
	get_option(Options,sequence_index,_),
	get_option(Options,input_format,text(fasta_single(SequenceType))),
	get_option(Options,output_format,text(prolog(sequence(SequenceType)))).

% text(prolog(range)) ==> fasta (single sequence)
lost_output_format(annotate, Options, text(prolog(range(SequenceType)))) :-
	get_option(Options,output_format,text(prolog(range(SequenceType)))),
	get_option(Options,input_format,text(fasta_single(SequenceType))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main goal implementation rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% text(fasta(_)) ==> text(prolog(sequence(_))) 
annotate([InputFile], Options, OutputFile) :-
	get_option(Options,input_format,text(fasta(_))),
	get_option(Options,output_format,OutputFormat),
	get_option(Options,sequence_index,SeqId),
	% Load the fasta sequence:
	fasta_load_sequence(InputFile,SeqId,FastaHeaderLine,Sequence),
	atom_codes(HeaderLineAtom,FastaHeaderLine),
	% Save it in text(prolog(sequence(_))) format
	save_annotation_to_sequence_file(HeaderLineAtom, 32, Sequence, OutputFile).

% text(prolog(sequence(_)) ==> text(fasta(_))
annotate([InputFile], Options, OutputFile) :-
	get_option(Options,input_format,text(prolog(sequence(_)))),
	get_option(Options,output_format,text(fasta(_))),
	% Load the sequence from a file:
	load_annotation_from_file(sequence,[data_position(4)],InputFile,Data),
	% Save it in text(fasta(_)) format:
	atom_concat(' generated from file: ', InputFile, HeaderLine),
	fasta_save_sequence(OutputFile,Data,HeaderLine).
