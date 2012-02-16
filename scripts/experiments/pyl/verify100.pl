
candidate_pylis(ecoli10) <- file::get('file:///tmp/pylistest/10_ecoli_pylis.pl').
candidate_pylis(ecoli100) <- file::get('file:///tmp/pylistest/100_ecoli_pylis.pl').
candidate_pylis(known) <- file::get('file:///tmp/pylistest/known_pylis.pl').

merged_pylis <- append_all(member(X,[ecoli10,known]), candidate_pylis(X)).

folded <- ppfold::fold(merged_pylis, [sequence_functor(pylis)]).

folded_sorted <- ppfold::sort_folded_by_energy(folded).

fasta_sequences <- ranges::as_fasta(folded_sorted,[sequence_functor(pylis)]).

clustertree_naive <- rna_cluster::cluster_rna_foldings(folded_sorted).

clustertree_pmcomp <- rna_cluster::cluster_pmcomp(folded_sorted, [min_stem_length(4)]).

