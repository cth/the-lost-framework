# one interval per gene, no smoothing, regular EM
all_genes <- read.table('/tmp/range_probs_raw_2500.dat')
plot(all_genes$V2,t="l",ylab="interval probability", xlab="intervals",  main="all genes, e.g. 2495 intervals.")

# EM, no smoothing, 10 and 100 intervals
ranges_10 <- read.table('/tmp/range_probs_raw_10.dat')
plot(ranges_10$V2,t="l",ylab="interval probability", xlab="intervals", main="10 intervals, usual EM, no smoothing")
ranges_100 <- read.table('/tmp/range_probs_raw_100.dat')
plot(ranges_100$V2,t="l",ylab="interval probability", xlab="intervals",  main="100 intervals, usual EM, no smoothing")

ranges_100_smooth <- read.table('/tmp/range_probs_smooth_100.dat')
plot(ranges_100_smooth$V2,t="l", ylab="interval probability", xlab="intervals", main="100 intervals. Usual EM. smoothed once.")

ranges_250_vb <- read.table('/tmp/range_probs_250_vb.dat')
plot(ranges_250_vb$V2,t="l", ylab="interval probability", xlab="intervals", main="250 intervals. VB. vs salmonella ditto")
salmonella_ranges_250_vb <- read.table('/tmp/range_probs_salmonella_250_vb.dat')
lines(salmonella_ranges_250_vb$V2,t="l", ylab="interval probability", xlab="intervals",col=3) #, main="Salmonella. 250 intervals. VB.")

# Cross validation: 50 intervals
vb_50_xval1 <- read.table('/tmp/range_probs_50_vb_xval1.dat')
plot(vb_50_xval1$V2,t="l", ylab="interval probability", xlab="intervals", main='X-validation. 50 intervals. VB.')
vb_50_xval2 <- read.table('/tmp/range_probs_50_vb_xval2.dat')
lines(vb_50_xval2$V2,t="l")
vb_50_xval3 <- read.table('/tmp/range_probs_50_vb_xval3.dat')
lines(vb_50_xval3$V2,t="l")


# Cross validation: 25 intervals
vb_25_xval1 <- read.table('/tmp/range_probs_25_vb_xval1.dat')
plot(vb_25_xval1$V2,t="l", ylab="interval probability", xlab="intervals", main='X-validation. 25 intervals. VB.')
vb_25_xval2 <- read.table('/tmp/range_probs_25_vb_xval2.dat')
lines(vb_25_xval2$V2,t="l")
vb_25_xval3 <- read.table('/tmp/range_probs_25_vb_xval3.dat')
lines(vb_25_xval3$V2,t="l")

# Cross validation: 20 intervals
vb_20_xval1 <- read.table('/tmp/range_probs_20_vb_xval1.dat')
plot(vb_20_xval1$V2,t="l",ylab="interval probability", xlab="intervals", main='X-validation. 20 intervals. VB.')
vb_20_xval2 <- read.table('/tmp/range_probs_20_vb_xval2.dat')
lines(vb_20_xval2$V2,t="l")
vb_20_xval3 <- read.table('/tmp/range_probs_20_vb_xval3.dat')
lines(vb_20_xval3$V2,t="l")


# Cross validation: 10 intervals
vb_10_xval1 <- read.table('/tmp/range_probs_10_vb_xval1.dat')
plot(vb_10_xval1$V2,t="l", ylab="interval probability", xlab="intervals", main='X-validation. 10 intervals. VB.'	)
vb_10_xval2 <- read.table('/tmp/range_probs_10_vb_xval2.dat')
lines(vb_10_xval2$V2,t="l")
vb_10_xval3 <- read.table('/tmp/range_probs_10_vb_xval3.dat')
lines(vb_10_xval3$V2,t="l")







