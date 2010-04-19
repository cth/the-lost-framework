:- ['../../lost.pl'].                                                                   
:- lost_include_api(interface).                                                         
%:- lost_include_api(misc_utils).
%:- lost_include_api(io).
:- lost_include_script(script_parser).
:- lost_include_api(utils_parser_report).                                                                                                                                        

lost_input_formats(lost_best_annotation, [text(fasta)]).
lost_output_format(lost_best_annotation, _Options, [text(prolog(ranges(gene)))]).

                                                                                        
% This is what is used to get the best annotation 
% requires translationmode{0,1} and gencodefile.                                      
lost_best_annotation([Chunk_File],Options,Chunk_Conservation_File) :-                                 
	write('LoSt chunk AA conservation analysis: '),nl,				
%writeln('interface pre 0'),
	cl('chunk_aa_conservation.pl'), % Load the actual PRISM model 
	lost_required_option(Options,nmScore,MismatchScore),
	lost_required_option(Options,direction,Dir),
	lost_required_option(Options,frame,Frame),
	(	member(alignments(AlnFile),Options) -> 
			assert(output_alignments(yes)),
			open(AlnFile, write, Aln_Stream, [alias(alnout)])
	;
			assert(output_alignments(no))
	),
											(output_alignments(yes) -> writeln('yes interface'); true),
%writeln('interface pre 1'),
	assert(nongap_mismatch_score(MismatchScore)),				
	open(Chunk_File,read,Chunk_Stream,[alias(chunk_in)]),
%writeq(open(Chunk_File,read,Chunk_Stream,[alias(chunk_in)])),nl,
	open(Chunk_Conservation_File,write,Cons_Stream,[alias(cons_out)]),
%writeq(open(Chunk_Conservation_File,write,Cons_Stream,[alias(cons_out)])),
%writeln('interface pre 2'),	
	conservation(Chunk_Stream,1,Dir,Frame, Aln_Stream, Cons_Stream),
%writeln('interface post 0'),
	nl(user_output), writeln(user_output,'...'),
	close(Cons_Stream),
	close(Chunk_Stream),
	retractall(nongap_mismatch_score(_)),
	(	member(alignements(AlnFile),Options) -> 
			retractall(output_alignments(_)),
			close(Aln_Stream)
	;
			true
	),					
	write('LoSt chunk AA conservation analysis completed succesfully'),nl.