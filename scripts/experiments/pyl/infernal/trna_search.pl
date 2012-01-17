
%% 
% Setting up an infernal model:
% This uses the aligned trnas to create a covariance model for searching
% genomes for similar tRNAs.

% Might need to alter this absolute path: 
trna_alignment <- file::get('file:///home/ctheilhave/lost/scripts/experiments/pyl/infernal/trna-pyl.sto').

infernal_model <- infernal::build(trna_alignment).

calibrated_model <- infernal::calibrate(infernal_model).


% Organisms of interest:

organism('Methanosarcina thermophila').
organism('Methanohalobium evestigatum').
organism('Methanosalsum zhilinae').
organism('Desulfotomaculum acetoxidans').
organism('Bilophilia wadsworthia').
organism('Acetohalobium arabaticum').
organism('Thermincola potens').
organism('Desulfosporosinus orientis').
organism('Desulfotomaculum gibsoniae').
organism('Desulfosporosinus meridiei').
organism('Bartonella rochalimae').
organism('Helicobacter suis').
% Same organism, but have two chromosomes:
organism('Burkholderia pseudomallei chr1'). % chromosome 1
organism('Burkholderia pseudomallei chr2'). % chromosome 2


%% 
% Getting the genomes:
%

% Methanohalophilus mahii
genome('Methanosarcina thermophila') <- file::get('ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Methanohalophilus_mahii_DSM_5219_uid30711/CP001994.fna').

% Methanohalobium evestigatum
% Note: Also have a plasmid.
genome('Methanohalobium evestigatum') <- file::get('ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Methanohalobium_evestigatum_Z_7303_uid37945/CP002069.fna').

% Methanosarcina thermophila
% Genome sequence does not seem to be available

%genome('Methanosarcina thermophila') <- fail. 


% Methanosalsum zhilinae
genome('Methanosalsum zhilinae') <- file::get('ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Methanosalsum_zhilinae_DSM_4017_uid40771/CP002101.fna').

% Desulfotomaculum acetoxidans
genome('Desulfotomaculum acetoxidans') <- file::get('ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Desulfotomaculum_acetoxidans_DSM_771_uid27947/CP001720.fna').


% Bilophilia wadsworthia
% Only available as contigs/scaffolds? in
% genbank/genomes/Bacteria_DRAFT/
%

% Acetohalobium arabaticum
genome('Acetohalobium arabaticum') <- file::get('ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Acetohalobium_arabaticum_DSM_5501_uid32769/CP002105.fna').

% Thermincola potens
genome('Thermincola potens') <- file::get('ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Thermincola_JR_uid41467/CP002028.fna').

% Desulfosporosinus orientis
genome('Desulfosporosinus orientis') <- file::get('ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Desulfosporosinus_orientis_DSM_765_uid66191/CP003108.fna').

% Desulfotomaculum gibsoniae
% Only available as contigs
% genome('Desulfotomaculum gibsoniae') <- file::get('')


% Desulfosporosinus meridiei
% Only available as contigs

% Bartonella rochalimae
% Not even published as DRAFT genome

% Helicobacter suis
% Only available contigs DRAFT genome (2 varieties)

% Burkholderia pseudomallei
% Many genomes available. The genomes each have two chromosomes.

genome('Burkholderia pseudomallei chr1') <- file::get('ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Burkholderia_pseudomallei_1106a_uid16182/CP000572.fna').
genome('Burkholderia pseudomallei chr2') <- file::get('ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Burkholderia_pseudomallei_1106a_uid16182/CP000573.fna').

%%
% Searching the genomes for tRNAs-pyl

trna_pyl_hits(Org) <- organism(Org) | infernal::search([calibrated_model,genome(Org)]).


% test
runall :- 
	findall(Organism,organism(Organism),Organisms),
	forall(member(Organism,Organisms), run(trna_pyl_hits(Organism))).


