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
% cluster 3 - mostly known pylis
gene('Thermincola potens_c3_gene1',34088,32574). 
gene('Acetohalobium arabaticum_c3_gene1',385142,386602).
gene('Desulfitobacterium_hafniense_DCB_2_uid205_c3_gene1',5192551,5191061).
gene('Methanococcoides_burtonii_DSM_6242_uid9634_c3_gene1',2438105,2439598).
gene('Methanococcoides_burtonii_DSM_6242_uid9634_c3_gene2',1461495,1460005).
gene('Methanohalophilus_mahii_DSM_5219_uid30711_c3_gene1',1670949,1669459).
gene('Methanosarcina_mazei_uid300_c3_gene1',2445655,2444177).
gene('Methanosarcina_mazei_uid300_c3_gene2',2031368,2032855).
gene('Methanosarcina_barkeri_fusaro_uid103_c3_gene1',1858475,1859959).
gene('Methanosalsum_zhilinae_DSM_4017_uid40771_c3_gene1',511852,513300).
gene('Methanosarcina_acetivorans_uid290_c3_gene1',1111302, 1109815). % unknown
gene('Methanosarcina_acetivorans_uid290_c3_gene2',616367,617884). % unknown
gene('Methanohalobium_evestigatum_Z_7303_uid37945_c3_gene1',1965371,1963884). % unknown

% cluster 1- mostly unknown pylis
gene('Methanohalophilus_mahii_DSM_5219_uid30711_c1_gene1',1153476,1152100). % unknown
gene('Methanohalophilus_mahii_DSM_5219_uid30711_c1_gene2',1169884,1168508). % unknown
gene('Methanosarcina_acetivorans_uid290_c1_gene1',3707674,3709050). % unknown
gene('Methanosalsum_zhilinae_DSM_4017_uid40771_c1_gene1',1642608,1641232). % unknown
gene('Methanococcoides_burtonii_DSM_6242_uid9634_c1_gene1',892610,891213). % unknown
gene('Methanohalobium_evestigatum_Z_7303_uid37945_c1_gene1',1343605,1342229). % unknown
gene('Desulfotomaculum_acetoxidans_DSM_771_uid27947_c1_gene1',25960,27348).
gene('Methanosarcina_barkeri_fusaro_uid103_c1_gene1',4670552,4669155). % unknown
gene('Methanosarcina_barkeri_fusaro_uid103_c1_gene2',1023856,1025232). % unknown
gene('Methanosarcina_barkeri_fusaro_uid103_c1_gene3',1021046,1022422). % unknown
gene('Methanosalsum_zhilinae_DSM_4017_uid40771_c1_gene1',677927,679303). % unknown
gene('Methanosalsum_zhilinae_DSM_4017_uid40771_c1_gene2',1639379,1638003). % unknown
gene('Methanococcoides_burtonii_DSM_6242_uid9634_c1_gene1',885076,883694). % unknown
gene('Methanosarcina_mazei_uid300_c1_gene1',4063387,4064763). % unknown
gene('Methanosarcina_mazei_uid300_c1_gene2',1718668,1717292). % unknown
gene('Methanosarcina_acetivorans_uid290_c1_gene1',172570,171194).
gene('Methanohalobium_evestigatum_Z_7303_uid37945_c1_gene1',1025102,1023726). % unknown
gene('Acetohalobium_arabaticum_c1_gene1',1812983,1811595). % unknown

% cluster 2 - mostly unknown pylis
gene('Acetohalobium_arabaticum_c1_gene1',1356698,1355301). % unknown
gene('Methanohalophilus_mahii_DSM_5219_uid30711_c1_gene1',517715,519118). % unknown
gene('Methanohalophilus_mahii_DSM_5219_uid30711_c1_gene2',1666354,1664951). % unknown
gene('Methanococcoides_burtonii_DSM_6242_uid9634_c1_gene1',2415898,2416758). % unknown
gene('Methanosarcina_barkeri_fusaro_uid103_c1_gene1',1862619,1864040). % unknown
gene('Methanosalsum_zhilinae_DSM_4017_uid40771_c1_gene1',652074,653498). % unknown
gene('Methanohalophilus_mahii_DSM_5219_uid30711_c1_gene2',1665333,1665166). % unknown - no UAG
gene('Methanohalobium_evestigatum_Z_7303_uid37945_c1_gene1',1432492,1431089). % unknown
gene('Methanosarcina_barkeri_fusaro_uid103_c1_gene1',4624960,4626390). % unknown
gene('Methanosarcina_acetivorans_uid290_c1_gene1',1113298,1111892). % unknown
gene('Methanosarcina_acetivorans_uid290_c1_gene2',3004114,3005523). % unknown
gene('Methanosarcina_acetivorans_uid290_c1_gene3',620599,622026). % unknown
gene('Methanococcoides_burtonii_DSM_6242_uid9634_c1_gene1',1456870,1455458). % unknown
gene('Methanosarcina_mazei_uid300_c1_gene1',2447685,2446279). % unknown
gene('Methanosarcina_mazei_uid300_c1_gene2',2035520,2037019). % unknown
gene('Methanosarcina_mazei_uid300_c1_gene3',3574111,3575520). % unknown
gene('Methanohalobium_evestigatum_Z_7303_uid37945_c1_gene1',1960730,1959327).

% not in cluster
gene('Desulfobacterium_autotrophicum_HRM2_uid20931_c0_gene1',64449,65876). % no UAG
% gene('Methanosarcina_barkeri_fusaro_uid103 - gene 2',3034,4410). % wrong

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Extracting relevant parts of the genomes and annotating the results 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Retrieve the genome
genome(G) <- genome_gene_link(G,Link) | file::get(Link).

% Extract the sequence of the the gene G
geneseq(G) <- 
        gene(G,Start,End),
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

% Find any in-frame amber codons in G and annotate it accordingly
gene_uag_annot(G) <- pyl::annotate_orfs_with_in_frame_stops(geneseq(G)).

% Annotate G with the sequence 100 bp downstream the in-frame uag
gene_pyl(G) <- pyl::add_downstream_inframe_stops_sequences( gene_uag_annot(G) ).

% Merge all PYL genes in one file
all_pyl_genes <- append_all(gene(G,_,_), gene_pyl(G)).


