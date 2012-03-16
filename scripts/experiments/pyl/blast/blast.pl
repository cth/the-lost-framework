genome_link('Thermincola potens','ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Thermincola_JR_uid41467/CP002028.fna').
genome_link('Acetohalobium arabaticum','ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Acetohalobium_arabaticum_DSM_5501_uid32769/CP002105.fna').
genome_link('Desulfitobacterium_hafniense_DCB_2_uid205', 'ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Desulfitobacterium_hafniense_DCB_2_uid205/CP001336.fna').
genome_link('Desulfobacterium_autotrophicum_HRM2_uid20931', 'ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Desulfobacterium_autotrophicum_HRM2_uid20931/CP001087.fna').
genome_link('Desulfotomaculum_acetoxidans_DSM_771_uid27947', 'ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Desulfotomaculum_acetoxidans_DSM_771_uid27947/CP001720.fna').
genome_link('Methanococcoides_burtonii_DSM_6242_uid9634', 'ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Methanococcoides_burtonii_DSM_6242_uid9634/CP000300.fna').
genome_link('Methanohalophilus_mahii_DSM_5219_uid30711', 'ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Methanohalophilus_mahii_DSM_5219_uid30711/CP001994.fna').
genome_link('Methanosarcina_acetivorans_uid290','ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Methanosarcina_acetivorans_uid290/AE010299.fna').
genome_link('Methanohalobium_evestigatum_Z_7303_uid37945','ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Methanohalobium_evestigatum_Z_7303_uid37945/CP002069.fna').
genome_link('Methanosarcina_mazei_uid300 - gene 1','ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Methanosarcina_mazei_uid300/AE008384.fna').
genome_link('Methanosarcina_mazei_uid300 - gene 2','ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Methanosarcina_mazei_uid300/AE008384.fna').
genome_link('Methanosarcina_barkeri_fusaro_uid103','ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Methanosarcina_barkeri_fusaro_uid103/CP000099.fna').
genome_link('Methanosalsum_zhilinae_DSM_4017_uid40771','ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Methanosalsum_zhilinae_DSM_4017_uid40771/CP002101.fna').

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

/*
go :-
		findall(Genome,genome_link(Genome,_Link),Genomes),
		foreach(DatabaseGenome in Genomes,
			foreach(QueryGenome in Genomes,
				run(blast_results_no_self_hits(DatabaseGenome,QueryGenome)))).
*/

hafniense :-
        run(hits_with_match('Acetohalobium arabaticum','Acetohalobium arabaticum')).

go :- run(all_results_sorted).
