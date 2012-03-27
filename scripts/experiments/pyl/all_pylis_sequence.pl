% Organisms of interest:
genome_link('Thermincola potens','ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Thermincola_JR_uid41467/CP002028.fna').
genome_link('Acetohalobium arabaticum','ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Acetohalobium_arabaticum_DSM_5501_uid32769/CP002105.fna').
genome_link('Desulfitobacterium_hafniense_DCB_2_uid205', 'ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Desulfitobacterium_hafniense_DCB_2_uid205/CP001336.fna').
genome_link('Desulfobacterium_autotrophicum_HRM2_uid20931', 'ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Desulfobacterium_autotrophicum_HRM2_uid20931/CP001087.fna').
genome_link('Desulfotomaculum_acetoxidans_DSM_771_uid27947', 'ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Desulfotomaculum_acetoxidans_DSM_771_uid27947/CP001720.fna').
genome_link('Methanococcoides_burtonii_DSM_6242_uid9634', 'ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Methanococcoides_burtonii_DSM_6242_uid9634/CP000300.fna').
genome_link('Methanohalophilus_mahii_DSM_5219_uid30711', 'ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Methanohalophilus_mahii_DSM_5219_uid30711/CP001994.fna').
genome_link('Methanosarcina_acetivorans_uid290','ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Methanosarcina_acetivorans_uid290/AE010299.fna').
genome_link('Methanohalobium_evestigatum_Z_7303_uid37945','ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Methanohalobium_evestigatum_Z_7303_uid37945/CP002069.fna').
genome_link('Methanosarcina_mazei_uid300','ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Methanosarcina_mazei_uid300/AE008384.fna').
genome_link('Methanosarcina_barkeri_fusaro_uid103','ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Methanosarcina_barkeri_fusaro_uid103/CP000099.fna').
genome_link('Methanosalsum_zhilinae_DSM_4017_uid40771','ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Methanosalsum_zhilinae_DSM_4017_uid40771/CP002101.fna').

% Connecting a gene to its genome
genome_gene_link(OrganismGene, Link) :-
	atom_codes(OrganismGene,OrgGeneCodes),
	genome_link(MatchOrganism,Link),
	atom_codes(MatchOrganism,MatchOrganismCodes),
	append(MatchOrganismCodes,_GeneId,OrgGeneCodes).

% Positions of known mttB genes:
% mtb_gene(-Organism,-Start,-End)
gene('Thermincola potens',34088,32574).
gene('Acetohalobium arabaticum',385142,386602).
gene('Desulfitobacterium_hafniense_DCB_2_uid205',5192551,5191061).
gene('Desulfobacterium_autotrophicum_HRM2_uid20931',64449,65876).
gene('Desulfotomaculum_acetoxidans_DSM_771_uid27947',25960,27348).
gene('Methanococcoides_burtonii_DSM_6242_uid9634 - gene 1',2438105,2439598).
gene('Methanococcoides_burtonii_DSM_6242_uid9634 - gene 2',1461495,1460005).
gene('Methanohalophilus_mahii_DSM_5219_uid30711',1670949,1669459).
gene('Methanosarcina_acetivorans_uid290',172570,171194).
gene('Methanohalobium_evestigatum_Z_7303_uid37945',1960730,1959327).
gene('Methanosarcina_mazei_uid300 - gene 1',2445655,2444177).
gene('Methanosarcina_mazei_uid300 - gene 2',2031368,2032855).
gene('Methanosarcina_barkeri_fusaro_uid103 - gene 1',1858475,1859959).
gene('Methanosarcina_barkeri_fusaro_uid103 - gene 2',3034,4410).
gene('Methanosalsum_zhilinae_DSM_4017_uid40771',511852,513300).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Extracting relevant parts of the genomes and annotating the results 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Retrieve the genome
genome(G) <- genome_gene_link(G,Link) | file::get(Link).

% Extract the sequence of the the gene G
geneseq(G) <- 
        mtb_gene(G,Start,End), 
        ((Start > End) ->
                ReverseComplement = true,
                Left = End,
                Right = Start
                ; 
                ReverseComplement = false,
                Left = Start,
                Right = End
        )
        | 
        sequence::extract(genome(G),[left(Left),right(Right),reverse_complement(ReverseComplement),sequence_identifier(G)]).

% Merge all genes in one file
all_genes <- append_all(gene(G,_,_), geneseq(G)).

