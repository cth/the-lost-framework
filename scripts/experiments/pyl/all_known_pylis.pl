%
% Create a clustering of the known mtb genes to see what such a cluster would look like

% Organisms of interest:
%
%

:- use(genedb).

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

gene_uag_annot(G) <-  pyl::annotate_orfs_with_in_frame_stops(geneseq(G)).

gene_pyl_all(G) <- pyl::add_downstream_inframe_stops_sequences(  gene_uag_annot(G) ).

gene_pyl_valid(G) <- pyl::candidate_pylis(gene_uag_annot(G)).

all_pyl_genes <- append_all(genome_link(X,_), gene_pyl_valid(X)).

all_pylis_fasta <- ranges::as_fasta(all_pyl_genes, [sequence_functor(pylis)]). 

go :-
        run(all_pyl_genes).

gene_in_frames :-
        get_result_file(all_pyl_genes, File),
        writeln('---------------'),
        terms_from_file(File,Terms),
        %findall([Org,Stops],(member(G,Terms),gene_sequence_id(G,Org),gene_extra_field(G,in_frame_stops,Stops)),NumInframes),
        forall(member(G,Terms),(gene_sequence_id(G,Org),gene_extra_field(G,in_frame_stops,Stops),write(Org),write('\t'),writeln(Stops))).

aug_downstreams :-
        get_result_file(all_pyl_genes, File),!,
        writeln('---------------'),!,
        terms_from_file(File,Terms),
        forall(member(G,Terms),(gene_sequence_id(G,Org),gene_extra_field(G,downstream_stops,Down),writeln(Down))).


mtb_gene_length :-
       forall(mtb_gene(X,L,R), (Len is abs(L-R), write(X), write('\t\t\t'),writeln(Len))). 


%%%%%%%%%%%%%%%%%%%%%
% PART 2
%

genome(e_coli) <- file::get('ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Escherichia_coli_K_12_substr__MG1655_uid225/U00096.fna').

candidate_pyl_orfs(X) <- pyl::candidate_orfs(genome(X), [sequence_identifier(X)]).

candidate_pylis(X) <- pyl::candidate_pylis(candidate_pyl_orfs(X)).

%candidate_pylis_as_fasta(X) <- ranges::as_fasta(candidate_pylis(X),[sequence_functor(pylis)]).

%% Folding of sequences

folded(X) <- ppfold::fold(candidate_pylis(X), [sequence_functor(pylis)]).

folded_sorted(X) <- ppfold::sort_folded_by_energy(folded(X)).

pylis_cand_ecoli(X) <- integer(X) | ranges::take(folded_sorted(e_coli), [count(X)]).
pylis_cand_ecoli_100 <- ranges::take(folded_sorted(e_coli), [count(10)]).




%
