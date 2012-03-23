:- task(candidate_orfs([text(fasta)],[sequence_identifier(na)],text(prolog(ranges(gene))))).

:- task(annotate_orfs_with_in_frame_stops([text(prolog(ranges(gene)))],[], text(prolog(ranges(gene))))).

:- task(add_downstream_inframe_stops_sequences([text(prolog(ranges(gene)))],[max_bases_downstream(100)], text(prolog(ranges(gene))))).

:- task(candidate_pylis([text(prolog(ranges(gene)))],[extract_size(100)], text(prolog(ranges(gene))))).

:- task(filter_pylis_orfs_by_overlaps([text(prolog(ranges(gene))),text(prolog(ranges(gene)))],[max_overlap(100)], text(prolog(ranges(gene))))).

:- task(hits_matching_pylis_orfs([text(prolog(ranges(gene))),text(prolog(ranges(gene)))],[min_overlap(100)], text(prolog(ranges(gene))))).

:- task(hits_rna_match([text(prolog(ranges(gene))),text(prolog(ranges(gene)))],[], text(prolog(ranges(gene))))).

:- task(trim_blast_hits([text(prolog(ranges(gene)))],[], text(prolog(ranges(gene))))).

:- task(hit_clusters([text(prolog(ranges(gene)))],[], [text(prolog(ranges(gene))),text(prolog(ranges(gene)))])).


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


%% filter_pylis_by_overlaps(+InputFiles,+Options,+OutputFile)
% ==
% InputFiles = [PylisCandidateOrfsFile,KnownGenesFile]
% ==
% Given a file with candidate pylis ORFs, =|PylisCandidateOrfsFile|= and a file containing known genes,
% =|KnownGenesFile|=, extract the pylis candidate ORFs which satisfy certain constraints about the overlap
% with existing genes.
filter_pylis_orfs_by_overlaps([OrfsFile,KnownGenesFile],Options,OutputFile) :-
	get_option(Options,max_overlap,MaxOverlap),
	cl(overlap_filter),
	filter_by_overlap(MaxOverlap,OrfsFile,KnownGenesFile,OutputFile).
	

%% hits_matching_list
% ==
%  InputFiles = [Hits,MustMatchList]
% ==
% For each hit in Hits, check if the hit corresponds to a range in the MustMatchList.
% Only hits that does match, are written to OutputFile
hits_matching_pylis_orfs([HitListFile,MustMatchListFile],Options,OutputFile) :-
	get_option(Options,min_overlap,MinOverlap),
	cl(hit_match),
	hit_match(MinOverlap,HitListFile,MustMatchListFile,OutputFile).

%% hits_rna_match(+InputFiles,+Options,+OutputFile)
% == 
% InputFiles = [ HitListFile, RNAFile ]
% ==
% For each hit, check if it overlaps a known RNA in =|RNAFile|=. If so, add an extra field
% rna_match(RNA).
hits_rna_match([HitListFile,RNAFile],_Options,OutputFile) :-
	cl(hit_match),
	no_rna_overlap(HitListFile,RNAFile,OutputFile).
	
%% trim_blast_hits(+InputFiles,+Options,+OutputFile)
% == 
% InputFiles = [ HitListFile ]
% ==
% Blast hits will have both the organism name and the position in the sequence id of each hit. 
% this task removes everything but the organism name
trim_blast_hits([InputFile],_Options,OutputFile) :-
	cl(trim_hits),
	trim_sequence_identifier(InputFile,OutputFile).

%% hit_clusters(+InputFiles,+Options,+OutputFiles)
% ==
% InputFiles = [ HitListFile]
% ==
% and 
% == 
% OutputFiles = [ ClustersSimple, ClustersDetailed ]
% ==
hit_clusters([InputFile],_Options,[ClustersSimple,ClustersDetailed]) :-
	cl(hit_closure),
	hit_closure(InputFile,ClustersSimple,ClustersDetailed).