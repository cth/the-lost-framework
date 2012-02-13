:- task(candidate_orfs([text(fasta)],[sequence_identifier(na)],text(prolog(ranges(gene))))).

:- task(candidate_pylis([text(prolog(ranges(gene)))],[extract_size(100)], text(prolog(ranges(gene))))).

:- task(candidate_pylis([text(prolog(ranges(gene)))],[extract_size(100)], text(prolog(ranges(gene))))).

:- task(add_downstream_inframe_stops_sequences([text(prolog(ranges(gene)))],[max_bases_downstream(100)], text(prolog(ranges(gene))))).

%% candidate_orfs(+InputFiles,+Options,+OutputFile)
% ==
% InputFiles = [ GenomeFastaFile ]
% ==
% Extract ORFs from GenomeFastaFile which have an UAG stop codon inframe.
% The ORFs must be of at least 60 bp
candidate_orfs([GenomeFastaFile],Options,OutputFile) :-
	cl(extract_orfs),
	get_option(Options,sequence_identifier,SeqId),
	extract_orfs(SeqId,GenomeFastaFile,OutputFile).

%% annotate_orfs_with_in_frame_stops(+InputFiles,+Options,+OutputFile)
% == 
% InputFiles = [ OrfsFile ]
% ==
% Annotate existing ORFs with a =|sequence|= extra field with an additional extra field
% =|in_frame_stops|= that contains a list of the positions of all in frame amber codons.
annotate_orfs_with_in_frame_stops([OrfsFile],_Options,OutputFile) :-
	cl(annotate_existing_orfs),
	annotate_orfs(OrfsFile,OutputFile).
	
%% add_downstream_inframe_stops_sequences(+InputFiles,+Options,+OutputFile)
% == 
% InputFiles = [ OrfsFile ]
% ==
% Annotate existing ORFs with a =|downstream_stops|= extra field containing a list 
% of sequences downstream the stop. The list order corresponds to the the field =|in_frame_stops|=
% that contains a list of the positions of all in frame amber codons.
add_downstream_inframe_stops_sequences([OrfsFile],Options,OutputFile) :-
	get_option(Options,max_bases_downstream,MaxBasesDownstream),
	cl(add_pylis_seq),
	inframe_stop_sequences(OrfsFile,OutputFile,MaxBasesDownstream).

%% candidate_pylis(+InputFiles,+Options,+OutputFile)
% ==
% InputFiles = [ OrfsFile ]
% ==
% Given a file with candidate orfs (with in frame amber codons), for each amber codon,
% extract the a portion of the sequence downstream of the codon that may contain a PYLIS element. 
% The exact size of the downstream sequence element is configurable.
candidate_pylis([OrfsFile],_Options,OutputFile) :-
	cl(candidate_pylis),
	extract_candidate_pylis(OrfsFile,OutputFile).
