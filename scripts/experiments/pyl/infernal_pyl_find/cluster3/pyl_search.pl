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
genome_link('Desulfosporosinus_orientis_DSM_765_uid66191','ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Desulfosporosinus_orientis_DSM_765_uid66191/CP003108.fna').
genome_link('Geodermatophilus_obscurus_DSM_43160_uid29547','ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Geodermatophilus_obscurus_DSM_43160_uid29547/CP001867.fna').


%% 
% Setting up an infernal model:
% This uses the aligned trnas to create a covariance model for searching
% genomes for similar tRNAs.
%

genome(Genome) <- genome_link(Genome,Link) | file::get(Link).

% Might need to alter this absolute path: 
structure_alignment(ClusterFile) <- 
        working_directory(Here), 
        atom_concat(Here, '/', Dir),
        atom_concat(Dir,ClusterFile,Alignment) 
        | 
        file::get(file(Alignment)).

infernal_model(ClusterFile) <- infernal::build(structure_alignment(ClusterFile)).

calibrated_model(ClusterFile) <- infernal::calibrate(infernal_model(ClusterFile)).

%%
% Searching the genomes for tRNAs-pyl

pyl_hits(Org,ClusterFile) <- infernal::search([calibrated_model(ClusterFile),genome(Org)]).

go :-
        ClusterFile = 'mxscarna.sto',
        findall(G,genome_link(G,_), Genomes),
        foreach(Genome in Genomes, run(pyl_hits(Genome,ClusterFile))),
        % write file names:
        findall([Genome,ClusterFile,ResultFile],(member(Genome,Genomes),get_result_file(pyl_hits(Genome,ClusterFile),ResultFile)),ResultFiles),
        forall(member([A,B,C],ResultFiles), 
               (write(B), write('\t'),
                write(A), write('\t'),
                write(C), nl)),
        writeln(Genomes), 
        writeln('done').

write_result_file(ClusterFile,Genome) :-
        get_result_file(pyl_hits(Genome,ClusterFile),File),
        writeln(File).

