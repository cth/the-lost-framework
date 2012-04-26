%
% Create a list of genes annotated with UAG inside 
%

%:- debug(off).
:- use(genedb).

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
organism_link('Desulfosporosinus_orientis_DSM_765_uid66191','ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Desulfosporosinus_orientis_DSM_765_uid66191/CP003108').

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

genome_fasta(Genome) <- genome_link(Genome,URL) | file::get(URL).

genes_with_sequence(Organism) <- ranges::add_sequence_field([genes(Organism),genome_fasta(Organism)]).

genes_uag_annot(Organism) <-  pyl::annotate_orfs_with_in_frame_stops(genes_with_sequence(Organism)).

all_genes_with_uag <- append_all(organism_link(O,_),genes_uag_annot(O)).


test :-
        run(genes_uag_annot('Thermincola potens')).
