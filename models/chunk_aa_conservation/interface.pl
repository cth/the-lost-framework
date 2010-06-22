:- ['../../lost.pl'].
:- lost_include_api(interface).
:- lost_include_api(misc_utils).
:- lost_include_api(io).
:- lost_include_script(script_parser).
:- lost_include_api(utils_parser_report).


lost_input_formats(annotate, [text(fasta(ffa))]).
lost_output_format(annotate, _Options, text(prolog(ranges(gene)))).

lost_option(annotate,mismatch_score,1,'Non Gap Mismatch Score'). 
lost_option(annotate,direction,''). %+ for forward strand and - for reverse strand').
lost_option(annotate,frame,'Reading frame: 1,2 or 3').
lost_option(annotate,optimized,false,'Set a parallel compputation of the prediction').

% TODO: output best alignment = record in a separate file the best alignment /TODO 

lost_option_values(annotate,direction,['+','-']).
lost_option_values(annotate,frame,[1,2,3]).

% This is what is used to get the best annotation
% requires gencodefile.
annotate([Chunk_File],Options,Chunk_Conservation_File) :-
        get_option(Options,optimized,false),
        !,
	write('LoSt chunk AA conservation analysis: '),nl,
        cl('chunk_aa_conservation.pl'), % Load the actual PRISM model
	get_option(Options,mismatch_score,Mismatch_Score),
	get_option(Options,direction,Dir),
	get_option(Options,frame,Frame),
	(member(alignments(AlnFile),Options) ->  % Måske, not necessary TO CHECK if Option to declare
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



annotate([InputFile],Options,OutputFile) :-
	get_option(Options,optimized,true),
	subtract(Options,[optimized(true)],NewOptions1),
	append([optimized(false)], NewOptions1, NewOptions2),
	terms_to_file('options.pl',[original_options(NewOptions2)]),
	lost_tmp_directory(Tmp),
	atom_concat(Tmp,'conservation_chunk',Prefix),
	split_file(InputFile, 100, Prefix, '.pl'),
	atom_concat(Tmp,'conservation_chunk*pl',InputFilePattern),
	atom_concat_list(['sh parallel_predict.sh ', OutputFile, ' ', InputFilePattern], Cmd),
	system(Cmd).

