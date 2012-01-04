combiner_length_table <- read.table('combiner_length_distribution.txt')
combiner_blast_table <- read.table('length_dist_codpref_w_blast.txt')
codpref_old_table <- read.table('length_dist_codpref_old.txt')


# Returns the probability (relative frequency of observed lengths)
relative_length_frequencies <- function (lengths) {
	freqs = c()
	for(i in 1:max(lengths)) { freqs <- c(freqs, sum(lengths == i))	}
	freqs[freqs > 0] / sum(freqs) # normalize freqs
}

all_genes_table <- read.table('range_probs_raw_2500.dat')
plot(all_genes_table$V2,t="l",ylab="interval probability", xlab="intervals",  main="one range per gene, e.g. 2495 intervals.")

plot(relative_length_frequencies(combiner_length_table$V1),t="l", ylab="relative prediction length frequency", xlab="lengths", main='combiner: codon preference with length annotation', col=2)
plot(relative_length_frequencies(combiner_blast_table$V1),t="l", ylab="relative prediction length frequency", xlab="lengths", main='combiner: codon preference with blast annotation', col=3)
plot(relative_length_frequencies(codpref_old_table$V1),t="l", ylab="relative prediction length frequency", xlab="lengths", main='codon preference with grammar', col=4)



plot(relative_length_frequencies(combiner_length_table$V1),t="l", ylab="relative prediction length frequency", xlab="lengths", main='all combined in one plot', col=2)
lines(relative_length_frequencies(combiner_blast_table$V1),t="l",  col=3)
lines(relative_length_frequencies(codpref_old_table$V1),t="l", col=4)

#hist(combiner_length_distribution$V1, breaks=1000)
