# one interval per gene, no smoothing, regular EM
all_genes <- read.table('range_probs_raw_2500.dat')
plot(all_genes$V2,t="l",ylab="interval probability", xlab="intervals",  main="one range per gene, e.g. 2495 intervals.")

all_orfs <- scan('ecoli_all_orf_lengths.dat')
plot(as.data.frame(table(all_orfs)))

# one interval per gene, no smoothing, regular EM
all_genes_zoom_200 <- read.table('range_probs_raw_2500_zoom.dat')
plot(all_genes_zoom_200$V2,t="l",ylab="interval probability", xlab="intervals",  main="first 200 ranges")

# EM, no smoothing, 10 and 100 intervals
ranges_10 <- read.table('range_probs_raw_10.dat')
plot(ranges_10$V2,t="b",ylab="interval probability", xlab="intervals", main="10 intervals, usual EM, no smoothing")
ranges_100 <- read.table('range_probs_raw_100.dat')
plot(ranges_100$V2,t="b",ylab="interval probability", xlab="intervals",  main="100 intervals, usual EM, no smoothing")

ranges_100_smooth <- read.table('range_probs_smooth_100.dat')
plot(ranges_100_smooth$V2,t="b", ylab="interval probability", xlab="intervals", main="100 intervals. Usual EM. smoothed once.")

ranges_250_vb <- read.table('range_probs_250_vb.dat')
plot(ranges_250_vb$V2,t="l", ylab="interval probability", xlab="intervals", main="250 intervals. VB. vs salmonella ditto")
salmonella_ranges_250_vb <- read.table('range_probs_salmonella_250_vb.dat')
lines(salmonella_ranges_250_vb$V2,t="l", ylab="interval probability", xlab="intervals",col=3) #, main="Salmonella. 250 intervals. VB.")

# Cross validation: 50 intervals
vb_50_xval1 <- read.table('range_probs_50_vb_xval1.dat')
plot(vb_50_xval1$V2,t="l", ylab="interval probability", xlab="intervals", main='X-validation. 50 intervals. VB.')
vb_50_xval2 <- read.table('range_probs_50_vb_xval2.dat')
lines(vb_50_xval2$V2,t="l")
vb_50_xval3 <- read.table('range_probs_50_vb_xval3.dat')
lines(vb_50_xval3$V2,t="l")

# test
test1 <- read.table('test1.dat')
plot(test1$V2,t="l", ylab="interval probability", xlab="intervals", main='test1. 25 intervals. VB.')

# Cross validation: 25 intervals
vb_25_xval1 <- read.table('range_probs_25_vb_xval1.dat')
plot(vb_25_xval1$V2,t="l", ylab="interval probability", xlab="intervals", main='X-validation. 25 intervals. VB.')
vb_25_xval2 <- read.table('range_probs_25_vb_xval2.dat')
lines(vb_25_xval2$V2,t="l")
vb_25_xval3 <- read.table('range_probs_25_vb_xval3.dat')
lines(vb_25_xval3$V2,t="l")

# Cross validation: 20 intervals
vb_20_xval1 <- read.table('range_probs_20_vb_xval1.dat')
plot(vb_20_xval1$V2,t="l",ylab="interval probability", xlab="intervals", main='X-validation. 20 intervals. VB.')
vb_20_xval2 <- read.table('range_probs_20_vb_xval2.dat')
lines(vb_20_xval2$V2,t="l")
vb_20_xval3 <- read.table('range_probs_20_vb_xval3.dat')
lines(vb_20_xval3$V2,t="l")


# Cross validation: 10 intervals
vb_10_xval1 <- read.table('range_probs_10_vb_xval1.dat')
plot(vb_10_xval1$V2,t="l", ylab="interval probability", xlab="intervals", main='X-validation. 10 intervals. VB.'	)
vb_10_xval2 <- read.table('range_probs_10_vb_xval2.dat')
lines(vb_10_xval2$V2,t="l")
vb_10_xval3 <- read.table('range_probs_10_vb_xval3.dat')
lines(vb_10_xval3$V2,t="l")

adph <- read.table('adph2.dat')
plot(all_genes$V2,t="l",ylab="length probability", xlab="gene length",  main="gene length occurences vs acyclic phase model probability.")
lines(exp(adph$V2),t="b",col=3)

plot(exp(adph$V3), ylab="length viterbi probability", xlab="gene length", main="gene length acyclic phase model viterbi probabilities")



