

organism_url('ftp://ftp.ncbi.nih.gov/genomes/Bacteria/Escherichia_coli_K_12_substr__MG1655_uid57779/').

refseq_ptt <- 
	organism_url(X), 
	atom_concat(X,'NC_000913.ptt',TargetURL)
	| http::get(TargetURL).
	
known_genes <- ptt::parse(refseq_ptt).

viterbi_learn(1) <- simple_genefinder::learn(known_genes).

viterbi_learn(N) <- N > 1, N1 is N - 1 | simple_genefinder::learn(predict(N1)).

predict(N) <- simple_genefinder::predict(viterbi_learn(N)).