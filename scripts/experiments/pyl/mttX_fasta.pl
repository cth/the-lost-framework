% Organisms of interest:
genome_link('Thermincola_potens','ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Thermincola_JR_uid41467/CP002028.fna').
genome_link('Acetohalobium_arabaticum','ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Acetohalobium_arabaticum_DSM_5501_uid32769/CP002105.fna').
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
genome_link('Desulfosporosinus_orientis_DSM_765_uid66191','ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Desulfosporosinus_orientis_DSM_765_uid66191/CP003108.fna').


% Connecting a gene to its genome
genome_gene_link(OrganismGene, Link) :-
	atom_codes(OrganismGene,OrgGeneCodes),
	genome_link(MatchOrganism,Link),
	atom_codes(MatchOrganism,MatchOrganismCodes),
	append(MatchOrganismCodes,_GeneId,OrgGeneCodes).

cluster(1,[['Desulfosporosinus_orientis_DSM_765_uid66191',1161726,1163195,+,1,1162704],
['Acetohalobium_arabaticum',385082,386602,+,3,386129],
['Methanosarcina_mazei_uid300',2444168,2445655,-,3,2444656],
['Methanosarcina_mazei_uid300',2031368,2032855,+,3,2032367],
['Methanosarcina_acetivorans_uid290',1109815,1111302,-,2,1110303],
['Methanohalophilus_mahii_DSM_5219_uid30711',1669459,1671033,-,2,1669950],
['Methanococcoides_burtonii_DSM_6242_uid9634',2438078,2439598,+,3,2439107],
['Methanococcoides_burtonii_DSM_6242_uid9634',1460005,1461495,-,2,1460496],
['Desulfosporosinus_orientis_DSM_765_uid66191',1151158,1152678,+,2,1152166],
['Methanosarcina_acetivorans_uid290',616367,617884,+,3,617396],
['Methanosalsum_zhilinae_DSM_4017_uid40771',511645,513339,+,2,512851],
['Methanohalobium_evestigatum_Z_7303_uid37945',1963884,1965371,-,1,1964372],
['Methanosarcina_barkeri_fusaro_uid103',1858469,1859962,+,3,1859474],
['Desulfosporosinus_orientis_DSM_765_uid66191',4285515,4286993,-,1,4286012],
['Desulfitobacterium_hafniense_DCB_2_uid205',5191061,5192551,-,3,5191561],
['Desulfosporosinus_orientis_DSM_765_uid66191',1818581,1820053,+,3,1819562],
['Desulfosporosinus_orientis_DSM_765_uid66191',1712230,1713753,+,2,1713214],
['Thermincola_potens',32574,34106,-,1,33107]]).

cluster(2,[['Methanohalophilus_mahii_DSM_5219_uid30711',1152100,1153476,-,2,1152873],
['Methanohalophilus_mahii_DSM_5219_uid30711',1168508,1169884,-,3,1169281],
['Methanosarcina_acetivorans_uid290',3707674,3709050,+,2,3708277],
['Methanosalsum_zhilinae_DSM_4017_uid40771',677927,679303,+,3,678530],
['Methanococcoides_burtonii_DSM_6242_uid9634',891213,892610,-,1,891986],
['Methanohalobium_evestigatum_Z_7303_uid37945',1023726,1025102,-,1,1024499],
['Methanohalobium_evestigatum_Z_7303_uid37945',1342229,1343605,-,3,1343002],
['Desulfotomaculum_acetoxidans_DSM_771_uid27947',25951,27348,+,2,26566],
['Methanosarcina_barkeri_fusaro_uid103',4669155,4670552,-,1,4669928],
['Methanosarcina_barkeri_fusaro_uid103',1023856,1025232,+,2,1024459],
['Methanosarcina_barkeri_fusaro_uid103',1021046,1022422,+,3,1021649],
['Methanosalsum_zhilinae_DSM_4017_uid40771',1638003,1639379,-,1,1638776],
['Methanosalsum_zhilinae_DSM_4017_uid40771',1641232,1642608,-,2,1642005],
['Methanococcoides_burtonii_DSM_6242_uid9634',883694,885076,-,3,884467],
['Methanosarcina_mazei_uid300',4063387,4064763,+,2,4063990],
['Methanosarcina_mazei_uid300',1717292,1718668,-,3,1718065],
['Methanosarcina_acetivorans_uid290',171194,172570,-,3,171967],
['Desulfosporosinus_orientis_DSM_765_uid66191',1695269,1696660,+,3,1695878],
['Acetohalobium_arabaticum',1811595,1812983,-,1,1812377]]).

cluster(3,[['Desulfosporosinus_orientis_DSM_765_uid66191',1148959,1150428,+,2,1150084],
['Methanohalophilus_mahii_DSM_5219_uid30711',517715,519118,+,3,518780],
['Methanohalophilus_mahii_DSM_5219_uid30711',1664951,1666354,-,3,1665289],
['Methanococcoides_burtonii_DSM_6242_uid9634',2415898,2416758,+,2,2416420],
['Methanosarcina_barkeri_fusaro_uid103',1862619,1864040,+,1,1863702],
['Methanosalsum_zhilinae_DSM_4017_uid40771',652074,653498,+,1,653163],
['Methanohalophilus_mahii_DSM_5219_uid30711',1665166,1665333,-,2,1665285],
['Methanohalobium_evestigatum_Z_7303_uid37945',1431089,1432492,-,3,1431427],
['Methanohalobium_evestigatum_Z_7303_uid37945',1959327,1960730,-,1,1959665],
['Acetohalobium_arabaticum',1355301,1356698,-,1,1355639],
['Methanosarcina_barkeri_fusaro_uid103',4624960,4626390,+,2,4626052],
['Methanosarcina_acetivorans_uid290',1111892,1113298,-,3,1112233],
['Methanosarcina_acetivorans_uid290',3004114,3005523,+,2,3005185],
['Methanosarcina_acetivorans_uid290',620599,622026,+,2,621688],
['Methanococcoides_burtonii_DSM_6242_uid9634',1455458,1456870,-,3,1455796],
['Methanosarcina_mazei_uid300',2446279,2447685,-,2,2446620],
['Methanosarcina_mazei_uid300',2035520,2037019,+,3,2036681],
['Methanosarcina_mazei_uid300',3574111,3575520,+,2,3575182]]).


gene(GeneName,Start,End) :-
        cluster(Id,Members),
        member(Gene,Members),
        Gene = [ Organism, Left, Right, Strand, _Frame, _UAG ],
        ((Strand == +) -> 
                Start = Left,
                End = Right
                ;
                Start = Right,
                End = Left),
        atom_concat_list([Organism, '_c', Id, '_', Left, '_', Right],GeneName).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Extracting relevant parts of the genomes and annotating the results 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Retrieve the genome
genome(G) <- genome_gene_link(G,Link) | file::get(Link).

% Extract the sequence of the the gene G
geneseq(gene(G,Start,End)) <- 
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

%% Find any in-frame amber codons in G and annotate it accordingly
gene_uag_annot(G) <- pyl::annotate_orfs_with_in_frame_stops(geneseq(G)).

%% Annotate G with the sequence 100 bp downstream the in-frame uag
gene_pyl(G) <- pyl::add_downstream_inframe_stops_sequences( gene_uag_annot(G) ).

all_geneseq <- append_all(gene(G,S,E), geneseq(gene(G,S,E))).

%% Merge all PYL genes in one file
all_pyl_genes <- append_all(gene(G,S,E), gene_pyl(gene(G,S,E))).

fasta <- ranges::as_fasta(all_pyl_genes, [sequence_functor(pylis_sequence)]).

