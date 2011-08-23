% Produce a list of hard to find genes in the ecoli genome

%:- ['../../../lost.pl'].
%:- lost_include_api(lost_script).
%:- lost_include_api(interface).

organism_url('ftp://ftp.ncbi.nih.gov/genomes/Bacteria/Escherichia_coli_K_12_substr__MG1655_uid57779/').

gene_finder(prodigal).
gene_finder(glimmer).
gene_finder(genemark25).
gene_finder(genemark_hmm).
gene_finder(easygene).

report(prodigal) <- 
	organism_url(X),
	atom_concat(X,'NC_000913.Prodigal-2.50',TargetURL)
	| http::get(TargetURL).

report(glimmer) <- 
	organism_url(X), 
	atom_concat(X,'NC_000913.Glimmer3',TargetURL)
	| http::get(TargetURL).
	
report(genemark25) <-
	organism_url(X), 
	atom_concat(X,'NC_000913.GeneMark-2.5m',TargetURL)
	| http::get(TargetURL).
	
report(genemark_hmm) <-
	organism_url(X), 
	atom_concat(X,'NC_000913.GeneMarkHMM-2.6r',TargetURL)
	| http::get(TargetURL).

report(easygene) <-
	http::get('http://servers.binf.ku.dk/cgi-bin/easygene/output?id=60&format=gff&desc=CDS&desc=ORF&desc=ALT&r_cutoff=2&start=&stop=&strand=plus&strand=minus&per_page=100&183=Submit+Query').

refseq_ptt <- 
	organism_url(X), 
	atom_concat(X,'NC_000913.ptt',TargetURL)
	| http::get(TargetURL).
	
reference_genes <- ptt::parse(refseq_ptt).

genome_sequence_fasta <- organism_url(X), atom_concat(X,'NC_000913.fna',URL) | http::get(URL).

genome_sequence <- parser_fna::parse(genome_sequence_fasta,[list(256)]).


% Parse gene finder reports to yield predictions as Prolog facts

predictions(prodigal) <- prodigal::parse(report(prodigal)).

predictions(glimmer) <- glimmer3::parse(report(glimmer)).

predictions(genemark25) <- genemark::parse(report(genemark25)).

predictions(genemark_hmm) <- genemark_hmm::parse(report(genemark_hmm)).

predictions(easygene) <- easygene::parse(report(easygene)).

% Accuracy reports
accuracy(GF) <- gene_finder(GF) | accuracy_report::annotate(reference_genes,predictions(GF)).

evasive(GF) <- hard_to_find_genes::annotate(reference_genes, predictions(GF)).

evasive_rank <- 
	findall(predictions(GF),gene_finder(GF),GFResults)
	| hard_to_find_genes::annotate([reference_genes|GFResults],[sort(difficulty)]).
	
evasive <- gene_filter::annotate(evasive_rank, [exact_no_match_extra_fields([gene_finding_difficulty_score(1.0)])]).
non_evasive <- gene_filter::annotate(evasive_rank, [exact_match_extra_fields([gene_finding_difficulty_score(1.0)])]).

evasive_gene_stats <- range_stats::annotate([evasive, genome_sequence], [max_nucleotide_order(3),max_amino_acid_order(1)]).

evasive_tab_stats <- tabstats::tabstats(evasive_gene_stats).
non_evasive_tab_stats <- tabstats::tabstats(non_evasive_gene_stats).

non_evasive_gene_stats <- range_stats::annotate([non_evasive, genome_sequence], [max_nucleotide_order(3),max_amino_acid_order(1)]).



