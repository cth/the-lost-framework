:- ['../../lost.pl'].
:- [stop_trimmer].

lost_input_formats(trim_stops, [text(prolog(blast_matches))]).
lost_output_format(trim_stops,_, [text(prolog(blast_matches))]).

%%
% Trim stops in matches
% 
% Basically, finds the maximal suffix of all HSPs that do
% contain stop codons 
trim_stops([BlastHspFile],_Options,TrimmedHspFile) :-
	open(BlastHspFile,read,InStream),
	open(TrimmedHspFile,write,OutStream),
	stop_trimmer_rec(InStream,OutStream),
	close(InStream),
	close(OutStream).
