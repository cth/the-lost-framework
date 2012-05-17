
orf_lengths <- read.table('orf_lengths.dat')
organisms <- read.table('organisms.dat')
diversity <- read.table('diversity.dat')
codon_scores <- read.table('codon_score.dat')
upstream_uag <- read.table('avg_upstream_uag.dat')
downstream_uag <- read.table('avg_downstream_uag.dat')
diversity_ratio <- read.table('diversity_ratio.dat')
pmcomp <- read.table('pmcomp.dat')
syn_codons <- read.table('syn_codons.dat')

summary(diversity_ratio)

#combined_scores <- read.table('combined_scores.dat')

real.orf_lengths <- c(1514.329999999999927,1320.329999999999927,1382.049999999999955,1446.1875,1479.0,691.5)
real.diversity <- c(33.1983,19.8406,26.555,6.37333,1.0,19.0)
real.organisms <- c(11,9,10,2,1,2)
real.codon_scores <- c(111.829999999999998,107.100999999999999,95.234399999999994,103.179000000000002,56.011600000000001,58.296399999999998)
real.upstream_uag <- c(1016.610000000000014,992.11099999999999,605.368000000000052,293.1875,325.666999999999973,297.5)
real.downstream_uag <- c(494.944,325.444,773.158,1150.0,1149.67,391.0)
real.diversity_ratio <- c(33.198300000000003,19.840599999999998,26.555,6.37333,1.0,19.0)
real.pmcomp <- c(18.9969,37.585299999999997,22.5411,41.306399999999996,42.439999999999998,23.190000000000001)
real.syn_codons <- c(-0.078012,-0.126867,-0.0391984,0.340667,1.0,-0.2)
real.size <- c(18,18,19,16,3,2)


real.colors <- c("red","orange","yellow","green","purple","blue")

#combined <- data.frame(orf_lengths,organisms,diversity,codon_scores,upstream_uag,pmcomp,syn_codons)
#names(combined) <- c('iORF length','Organisms','Diversity','Codon score','Bases upstream UAG','Structure','syn')

#real.combined <- data.frame(real.orf_lengths,real.organisms,real.diversity,real.codon_scores,real.upstream_uag,real.pmcomp,real.syn_codons)
#names(real.combined) <- c('P-ORF length','Organisms','Diversity','Codon score','Bases upstream UAG','Structure','syn')

names <- c('Organisms','Diversity','Coding','Upstream','Downstream','Structure','syn_codons','ratio')

combined <- data.frame(organisms,diversity,codon_scores,upstream_uag,downstream_uag,pmcomp,syn_codons,diversity_ratio)
names(combined) <- names

real.combined <- data.frame(real.organisms,real.diversity,real.codon_scores,real.upstream_uag,real.downstream_uag,real.pmcomp,real.syn_codons,real.diversity_ratio)
names(real.combined) <- names

match_column <- function(df,datapoints) {
	match_index <- NULL
	for (i in 1:length(df[1,])) {
		column <- as.vector(df[,i])
		if (sum((column == datapoints)*1) == length(datapoints)) {
			match_index <- i
		}
	}
	match_index
}

my_panel_fun <- function(x,y,...) {
	points(x,y,...)
	
	x_i <- match_column(combined,x)
	y_i <- match_column(combined,y)
	
	points(real.combined[,x_i],real.combined[,y_i],pch=20,col=real.colors)
}

# check how many points are better than mean of positive examples
feature_test <- function(feature,feature_pos) {
	m <- mean(feature_pos)
	better <- feature[feature>mean(feature_pos)]
	which(feature>m)
	length(better)
}

pairs(combined,pch=1, panel=my_panel_fun)

a <- feature_test(downstream_uag,real.downstream_uag)
a

a <- feature_test(upstream_uag,real.upstream_uag)
a

a <- feature_test(upstream_uag*downstream_uag,real.upstream_uag*real.downstream_uag)
a

a <- feature_test(upstream_uag*codon_scores*real.downstream_uag,real.upstream_uag*real.codon_scores*real.downstream_uag)
a

a <- feature_test(codon_scores,real.codon_scores)
a

syn1 = abs(min(syn_codons)) + syn_codons 

syn1
real.syn1 = abs(min(syn_codons)) + real.syn_codons

div1 = abs(min(diversity)) + diversity 
real.div1 = abs(min(syn_codons)) + real.diversity

a <- feature_test(div1*syn1,real.div1*real.syn1)
a

a <- feature_test(pmcomp*upstream_uag,real.pmcomp*real.upstream_uag)
a

a <- feature_test(syn_codons*upstream_uag,real.syn_codons*real.upstream_uag)
a

a <- feature_test(codon_scores*upstream_uag,real.codon_scores*real.upstream_uag)
a

a <- feature_test(downstream_uag*upstream_uag,real.downstream_uag*real.upstream_uag)
a








