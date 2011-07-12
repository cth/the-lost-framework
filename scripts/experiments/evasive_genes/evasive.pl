% Produce a list of hard to find genes in the ecoli genome

%:- ['../../../lost.pl'].
%:- lost_include_api(lost_script).
%:- lost_include_api(interface).

organism_url('ftp://ftp.ncbi.nih.gov/genomes/Bacteria/Escherichia_coli_K_12_substr__MG1655_uid57779/').

prodigal_results <- 
	organism_url(X),
	atom_concat(X,'NC_000913.Prodigal-2.50',TargetURL)
	| http::get(TargetURL).

glimmer_results <- 
	organism_url(X), 
	atom_concat(X,'NC_000913.Glimmer3',TargetURL)
	| http::get(TargetURL).
	
	
genemark_25 <-
	organism_url(X), 
	atom_concat(X,'NC_000913.GeneMark-2.5m',TargetURL)
	| http::get(TargetURL).
	
genemark_hmm <-
	organism_url(X), 
	atom_concat(X,'NC_000913.GeneMarkHMM-2.6r',TargetURL)
	| http::get(TargetURL).


refseq_genes <- 
	organism_url(X), 
	atom_concat(X,'File:NC_000913.gbk',TargetURL)
	| http::get(TargetURL).
	



