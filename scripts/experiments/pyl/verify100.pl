candidate_pylis(ecoli10) <- file::get('file:///tmp/pylistest/10_ecoli_pylis.pl').
candidate_pylis(ecoli100) <- file::get('file:///tmp/pylistest/100_ecoli_pylis.pl').
candidate_pylis(known) <- file::get('file:///tmp/pylistest/known_pylis.pl').

candidate_pylis(ecoliall) <- file::get('file:///tmp/pylistest/ecoli_all.pl').

merged_pylis(10) <- append_all(member(X,[ecoli10,known]), candidate_pylis(X)).

merged_pylis(100) <- append_all(member(X,[ecoli100,known]), candidate_pylis(X)).

merged_coli_all <- append_all(member(X,[ecoliall,known]), candidate_pylis(X)).

%% folding:

folded(X) <- ppfold::fold(merged_pylis(X), [sequence_functor(pylis)]).

folded_sorted(X) <- ppfold::sort_folded_by_energy(folded(X)).

filter_folded(X) <- ppfold::filter_by_folding_constraints(folded_sorted(X),[min_energy(0.7)]).

as_fasta(X) <- ranges::as_fasta(folded_sorted(X)).

%% alignment:

aligned_edit <- constrained_align::align_edit_distance(folded_sorted(100)).

aligned_pmcomp <- constrained_align::align_pmcomp(folded_sorted(10)).

aligned_clustalw <- constrained_align::align_clustalw(folded_sorted(100)).

phylip_edit <- constrained_align::as_phylip_matrix(aligned_edit).

phylip_clustalw <- constrained_align::as_phylip_matrix(aligned_clustalw).


%% Clustering

rapidnj_edit <- rapidnj::cluster(phylip_edit).

rapidnj_clustalw <- rapidnj::cluster(phylip_clustalw).

clustertree_naive(X) <- rna_cluster::cluster_rna_foldings(folded_sorted(X)).

clustertree_pmcomp(X) <- rna_cluster::cluster_pmcomp(folded_sorted(X), [min_stem_length(4)]).

% Creating fasta files

fasta_all <- ranges::as_fasta(merged_coli_all,[sequence_functor(pylis)]).

fasta_sequences(X) <- ranges::as_fasta(folded_sorted(X),[sequence_functor(pylis)]).

