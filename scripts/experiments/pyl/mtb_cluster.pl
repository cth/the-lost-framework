%
% Create a clustering of the known mtb genes to see what such a cluster would look like

% Organisms of interest:
%

%:- debug(off).

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

% mtb_gene(-Organism,-Start,-End)
mtb_gene('Thermincola potens',34088,32574).
mtb_gene('Acetohalobium arabaticum',385142,386602).
mtb_gene('Desulfitobacterium_hafniense_DCB_2_uid205',5192551,5191061).
mtb_gene('Desulfobacterium_autotrophicum_HRM2_uid20931',64449,65876).
mtb_gene('Desulfotomaculum_acetoxidans_DSM_771_uid27947',25960,27348).
mtb_gene('Methanococcoides_burtonii_DSM_6242_uid9634',2438105,2439109).
mtb_gene('Methanohalophilus_mahii_DSM_5219_uid30711',1670949,1669459).
mtb_gene('Methanosarcina_acetivorans_uid290',172570,171194).
mtb_gene('Methanohalobium_evestigatum_Z_7303_uid37945',1960730,1959327).
mtb_gene('Methanosarcina_mazei_uid300 - gene 1',2444177,2445655).
mtb_gene('Methanosarcina_mazei_uid300 - gene 2',2031368,2032855).
mtb_gene('Methanosarcina_barkeri_fusaro_uid103',1858475,1859959).
mtb_gene('Methanosalsum_zhilinae_DSM_4017_uid40771',511852,513300).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The LoSt script for extracting relevant parts of the genomes and
% annotating the results 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

genome(G) <- genome_link(G,Link) | file::get(Link).

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

gene_uag_annot(G) <-  pyl_finder::annotate_orfs_with_in_frame_stops(geneseq(G)).

gene_pyl(G) <- pyl_finder::add_downstream_inframe_stops_sequences(  gene_uag_annot(G) ).



go :-
        forall(genome_link(X,_), run(gene_pyl(X))).
        
mtb_gene_length :-
       forall(mtb_gene(X,L,R), (Len is L-R, writeln(Len))). 
