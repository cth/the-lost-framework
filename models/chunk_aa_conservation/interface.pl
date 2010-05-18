:- ['../../lost.pl'].
:- lost_include_api(interface).
:- lost_include_api(misc_utils).
:- lost_include_api(io).
:- lost_include_script(script_parser).
:- lost_include_api(utils_parser_report).


lost_input_formats(lost_best_annotation, [text(fasta(ffa))]).
lost_output_format(lost_best_annotation, _Options, text(prolog(ranges(gene)))).

lost_option(lost_best_annotation,mismatch_score,1,'Non Gap Mismatch Score'). 
lost_option(lost_best_annotation,direction,''). %+ for forward strand and - for reverse strand').
lost_option(lost_best_annotation,frame,'Reading frame: 1,2 or 3').

% TODO: output best alignment = record in a separate file the best alignment /TODO 

lost_option_values(lost_best_annotation,direction,['+','-']).
lost_option_values(lost_best_annotation,frame,[1,2,3]).

% This is what is used to get the best annotation
% requires gencodefile.
lost_best_annotation([Chunk_File],Options,Chunk_Conservation_File) :-
	write('LoSt chunk AA conservation analysis: '),nl,
        cl('chunk_aa_conservation.pl'), % Load the actual PRISM model
	get_option(Options,mismatch_score,Mismatch_Score),
	get_option(Options,direction,Dir),
	get_option(Options,frame,Frame),
	(member(alignments(AlnFile),Options) ->  % M�ske, not necessary TO CHECK if Option to declare
			assert(output_alignments(yes)),
			open(AlnFile, write, Aln_Stream, [alias(alnout)])
	;
			assert(output_alignments(no))
	),
	%(output_alignments(yes) -> writeln('yes interface'); true),
	assert(nongap_mismatch_score(Mismatch_Score)),
	open(Chunk_File,read,Chunk_Stream,[alias(chunk_in)]),
	open(Chunk_Conservation_File,write,Cons_Stream,[alias(cons_out)]),
	conservation(Chunk_Stream,1,Dir,Frame,Aln_Stream,Cons_Stream),
	nl(user_output), writeln(user_output,'...'),
	close(Cons_Stream),
	close(Chunk_Stream),
	retractall(nongap_mismatch_score(_)),
	(member(alignements(AlnFile),Options) ->
            retractall(output_alignments(_)),
            close(Aln_Stream)
	;
            true
	),
	write('LoSt chunk AA conservation analysis completed succesfully'),nl.
