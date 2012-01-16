
genome_fasta <- file::get('ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Methanosarcina_barkeri_fusaro_uid103/CP000099.fna').

candidate_pyl_orfs <- pyl_finder::candidate_orfs(genome_fasta).