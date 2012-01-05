:- ['../../lost.pl'].

:- use(interface).

% Load the matcher code:
:- cl(matcher).

:- task(match([text(fasta)], [sequences([])], text(prolog(ranges(_))))).

%% match(+InputFiles,+Options,+OutputFile)
% == 
% InputFiles = [FastaFile]
% ==
% Searches for particular sequences in Fasta file on both strands
% Assumes that the genome is circular.
match([FastaFile],Options,OutputFile) :-
	get_option(Options,sequences,Sequences),
	writeln('Option sequences: '), writeln(Sequences),
	match_sequences(Sequences,FastaFile,OutputFile).
	