%
% Create a clustering of the known mtb genes to see what such a cluster would look like

% Organisms of interest:


% We will start with these two (but we need to find the rest!)

% Thermincola potens
genome('Thermincola potens') <- file::get('ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Thermincola_JR_uid41467/CP002028.fna').

mttB_gene('Thermincola potens') <- sequence::extract(genome('Thermincola potens'),[left(32574),right(34088),reverse_complement(true)]).

% Acetohalobium arabaticum
%genome('Acetohalobium arabaticum') <- file::get('ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Acetohalobium_arabaticum_DSM_5501_uid32769/CP002105.fna').




