
genome(m_barkeri) <- file::get('ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Methanosarcina_barkeri_fusaro_uid103/CP000099.fna').
genome(e_coli) <- file::get('ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Escherichia_coli_K_12_substr__MG1655_uid225/U00096.fna').
genome(hafniese) <- file::get('ftp://ftp.ncbi.nlm.nih.gov/genbank/genomes/Bacteria/Desulfitobacterium_hafniense_DCB_2_uid205/CP001336.fna').

candidate_pyl_orfs(X) <- pyl_finder::candidate_orfs(genome(X), [sequence_identifier(X)]).

candidate_pylis(X) <- pyl_finder::candidate_pylis(candidate_pyl_orfs(X)).

folded(X) <- ppfold::fold(candidate_pylis(X), [sequence_functor(pylis)]).

folded_sorted(X) <- ppfold::sort_folded_by_energy(folded(X)).

all :- 
        All = [ m_barkeri, e_coli, hafniese ],
        forall(member(M,All), run(candidate_pylis(M))),
        forall(member(M,All), run(folded_sorted(M))).

