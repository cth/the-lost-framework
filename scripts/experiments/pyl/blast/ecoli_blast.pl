genome_link(ecoli,'ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Escherichia_coli_K_12_substr__MG1655_uid225/U00096.fna').

genome_fasta(Genome) <- genome_link(Genome,URL) | file::get(URL).

% Extract all candidate p-orfs 
porfs_all_candidates(Genome) <- pyl::candidate_orfs(genome_fasta(Genome),[sequence_identifier(Genome)]).

% Filter all candidate p-orfs which have less than 100 bases downstream the UAG
porfs_long_candidates1(Genome) <- pyl::candidate_pylis(porfs_all_candidates(Genome)).

% Add the sequence downstream the UAG as an extra field to all candidates
porfs_long_candidates2(Genome) <- pyl::add_downstream_inframe_stops_sequences(porfs_long_candidates1(Genome)).

% Add a translated protein sequence of the the sequence downstream the UAG to all porfs
porfs_translated(Genome) <- ranges::translate(porfs_long_candidates2(Genome), [sequence_functor(pylis_sequence)]).

% Convert the protein sequence of all porfs to fasta file which functions as queries 
% to tblastn
porfs_blast_queries(Genome) <- ranges::as_fasta(porfs_translated(Genome),[sequence_functor(protein_sequence)]).

% Create a blast database from each genome
blastdb(Genome) <- blast::makeblastdb(genome_fasta(Genome)).

blast_results_xml(DatabaseGenome,QueryGenome) <- blast::tblastn([blastdb(DatabaseGenome),porfs_blast_queries(QueryGenome)]).

blast_results(DatabaseGenome,QueryGenome) <- blast::parse_xml(blast_results_xml(DatabaseGenome,QueryGenome)).

blast_results2(DatabaseGenome,QueryGenome) <- ranges::add_extra_field(blast_results(DatabaseGenome,QueryGenome),[extra_field(target_genome(DatabaseGenome))]).

blast_results_no_self_hits(DatabaseGenome,QueryGenome) <- blast::remove_self_hits(blast_results2(DatabaseGenome,QueryGenome)).

hits_with_match(DatabaseGenome,QueryGenome) <- pyl::hits_matching_pylis_orfs([blast_results_no_self_hits(DatabaseGenome,QueryGenome), porfs_long_candidates2(QueryGenome)], [min_overlap(90)]).

all_results <- append_all((genome_link(GenomeA,_),genome_link(GenomeB,_)),hits_with_match(GenomeA,GenomeB)).

all_results_sorted <- ranges::sort_by_field(all_results,[sort_field(evalue)]).

go :-
		findall(Genome,genome_link(Genome,_Link),Genomes),
		foreach(DatabaseGenome in Genomes,
			foreach(QueryGenome in Genomes,
				run(hits_with_match(DatabaseGenome,QueryGenome)))).

%go :- run(all_results_sorted).
