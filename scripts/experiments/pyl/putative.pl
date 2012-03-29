ptt <- file::get('ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Thermincola_JR_uid41467/CP002028.ptt').

genes <- ptt::parse(ptt,[genome_key(thermincola_potens)]).

% Exclude 'predicted/hypothetical/putative' etc genes
% The list of words is taken from easygene paper
safe_genes <- ranges::filter(genes, [regex_no_match_extra_fields([product("^.*(predicted|putative|unknown|possible|hypothetical|probable|bacteriophage|transposon|insertion|reverse transcriptase).*$")])]).
