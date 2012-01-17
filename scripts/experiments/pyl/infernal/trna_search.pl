
% Organism names:
organism(methanohalophilus_mahii).

%% 
% Setting up an infernal model:
% This uses the aligned trnas to create a covariance model for searching
% genomes for similar tRNAs.

% Might need to alter this absolute path: 
trna_alignment <- file::get('file:///home/ctheilhave/lost/scripts/experiments/pyl/infernal/trna-pyl.sto').

infernal_model <- infernal::build(trna_alignment).

calibrated_model <- infernal::calibrate(infernal_model).


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

organism(_).


%%
% Searching the genomes for tRNAs-pyl

trna_pyl_hits(Org) <- organism(Org) | infernal::search([genome(Org)).


% test



