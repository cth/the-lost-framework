organism_link('Thermincola potens','ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Thermincola_JR_uid41467/CP002028').
organism_link('Acetohalobium arabaticum','ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Acetohalobium_arabaticum_DSM_5501_uid32769/CP002105').
organism_link('Desulfitobacterium_hafniense_DCB_2_uid205', 'ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Desulfitobacterium_hafniense_DCB_2_uid205/CP001336').
organism_link('Desulfobacterium_autotrophicum_HRM2_uid20931', 'ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Desulfobacterium_autotrophicum_HRM2_uid20931/CP001087').
organism_link('Desulfotomaculum_acetoxidans_DSM_771_uid27947', 'ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Desulfotomaculum_acetoxidans_DSM_771_uid27947/CP001720').
organism_link('Methanococcoides_burtonii_DSM_6242_uid9634', 'ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Methanococcoides_burtonii_DSM_6242_uid9634/CP000300').
organism_link('Methanohalophilus_mahii_DSM_5219_uid30711', 'ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Methanohalophilus_mahii_DSM_5219_uid30711/CP001994').
organism_link('Methanosarcina_acetivorans_uid290','ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Methanosarcina_acetivorans_uid290/AE010299').
organism_link('Methanohalobium_evestigatum_Z_7303_uid37945','ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Methanohalobium_evestigatum_Z_7303_uid37945/CP002069').
organism_link('Methanosarcina_mazei_uid300','ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Methanosarcina_mazei_uid300/AE008384').
organism_link('Methanosarcina_barkeri_fusaro_uid103','ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Methanosarcina_barkeri_fusaro_uid103/CP000099').
organism_link('Methanosalsum_zhilinae_DSM_4017_uid40771','ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Methanosalsum_zhilinae_DSM_4017_uid40771/CP002101').

genome_link(Organism,GenomeLink) :-
        organism_link(Organism,OrganismLink),
        atom_concat(OrganismLink,'.fna',GenomeLink).

rna_link(Organism,RNALink) :-
        organism_link(Organism,OrganismLink),
        atom_concat(OrganismLink,'.rnt',RNALink).

genes_link(Organism,GenesLink) :-
        organism_link(Organism,OrganismLink),
        atom_concat(OrganismLink,'.ptt',GenesLink).

ptt(Organism) <- genes_link(Organism,Link) | file::get(Link).

genes(Organism) <- ptt::parse(ptt(Organism),[genome_key(Organism)]).

% Exclude 'predicted/hypothetical/putative' etc genes
% The list of words is taken from easygene paper
safe_genes(Organism) <- ranges::filter(genes(Organism),[regex_no_match_extra_fields([product("^.*(predicted|putative|unknown|possible|hypothetical|probable).*$")])]).

genome_fasta(Genome) <- genome_link(Genome,URL) | file::get(URL).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Training a codon model for each organism
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

genes_with_sequence(O) <- ranges::add_sequence_field(safe_genes(O),genome_fasta(O)).

training_genes(O) <- ranges::range_take(genes_with_sequence(O), [count(100)]).

codon_model(O) <- pyl::train_codon_model(training_genes(O)).


% Extract all candidate p-orfs 
porfs_01(Genome) <- pyl::candidate_orfs(genome_fasta(Genome),[sequence_identifier(Genome)]).

% Filter all candidate p-orfs which have less than 100 bases downstream the UAG
porfs_02(Genome) <- pyl::candidate_pylis(porfs_01(Genome)).

% Add the sequence downstream the UAG as an extra field to all candidates
porfs_03(Genome) <- pyl::add_downstream_inframe_stops_sequences(porfs_02(Genome)).

% Add a translated protein sequence of the the sequence downstream the UAG to all porfs
porfs_04(Genome) <- ranges::translate(porfs_03(Genome), [sequence_functor(pylis_sequence)]).

% Score each p-orf sequence using the codon model
porfs_05(Genome) <- pyl::score_with_codon_model(porfs_03(Genome),codon_model(Genome)).

% Get .rnt file containing rna genes for Genome
rnt_file(Genome) <- rna_link(Genome,URL) | file::get(URL).

% Parse .rnt file into prolog based representation
rnas(Genome) <- rnt::parse(rnt_file(Genome)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Reciprocal blast
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Convert the protein sequence of all porfs to fasta file which functions as queries 
% to tblastn
blast_queries(Genome) <- ranges::as_fasta(porfs_04(Genome),[sequence_functor(protein_sequence)]).

% Create a blast database from each genome
blastdb(Genome) <- blast::makeblastdb(genome_fasta(Genome)).

blast_results_xml(DatabaseGenome,QueryGenome) <- blast::tblastn([blastdb(DatabaseGenome),blast_queries(QueryGenome)]).

blast_results(DatabaseGenome,QueryGenome) <- blast::parse_xml(blast_results_xml(DatabaseGenome,QueryGenome)).

blast_results2(DatabaseGenome,QueryGenome) <- ranges::add_extra_field(blast_results(DatabaseGenome,QueryGenome),[extra_field(target_genome(DatabaseGenome))]).

blast_results_no_self_hits(DatabaseGenome,QueryGenome) <- blast::remove_self_hits(blast_results2(DatabaseGenome,QueryGenome)).

% Add the query orf (with full data) to each hit
hits_with_query(DatabaseGenome,QueryGenome) <- pyl::hits_match_query_orfs([blast_results_no_self_hits(DatabaseGenome,QueryGenome), porfs_05(QueryGenome)]).

hits_with_match(DatabaseGenome,QueryGenome) <- pyl::hits_matching_pylis_orfs([hits_with_query(DatabaseGenome,QueryGenome), porfs_05(DatabaseGenome)], [min_overlap(90)]).

blast_results_no_gene_overlaps(DatabaseGenome,QueryGenome) <- pyl::hits_no_gene_overlaps(hits_with_match(DatabaseGenome,QueryGenome),safe_genes(DatabaseGenome)).

blast_results_rna_overlaps(DatabaseGenome,QueryGenome) <- pyl::hits_rna_match([blast_results_no_gene_overlaps(DatabaseGenome,QueryGenome),rnas(DatabaseGenome)]).

all_results <- append_all((genome_link(GenomeA,_),genome_link(GenomeB,_)),blast_results_rna_overlaps(GenomeA,GenomeB)).

all_results_sorted <- ranges::sort_by_field(all_results,[sort_field(evalue)]).

results_trimmed <- pyl::trim_blast_hits(all_results_sorted).

clusters, clusters_detail <- pyl::hit_clusters(results_trimmed).

clusters(SortBy), clusters_detail(SortBy) <- pyl::rank_clusters([clusters,clusters_detail],[sort_by(SortBy)]).

all_results_different_sorted <- ranges::sort_by_field(all_results_different,[sort_field(evalue)]).


rerun_partial :-
        findall(Genome,genome_link(Genome,_Link),Genomes),
                foreach(DatabaseGenome in Genomes,
                        foreach(QueryGenome in Genomes, (
                                rerun(hits_with_match(DatabaseGenome,QueryGenome)),
                                rerun(blast_results_rna_overlaps(DatabaseGenome,QueryGenome))
                                )
                        )
                ),
        rerun(all_results),
        rerun(all_results_sorted).


go :- run(all_results_sorted).

go_test :-
        Org = 'Desulfobacterium_autotrophicum_HRM2_uid20931',
        run(hits_with_match(Org,Org)).

