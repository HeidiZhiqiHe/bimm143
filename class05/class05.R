#class 5 R

#2A Line plot
#read table "weight_chart"
weight <- read.table("bimm143_05_rstats/weight_chart.txt",header = TRUE )
#"weight_chart" with scatter plot
plot(weight[,1],weight[,2],pch=15, cex=1.5, lwd=2, ylim=c(2,10), xlab="Age(months)", ylab="Weight(kg)", main="age and weight", type = "b")

#2B Barplot
#read "feature_counts"
mouse<- read.table("bimm143_05_rstats/feature_counts.txt", header = TRUE, sep = "\t")
#"feature_counts" barplot
par(mar=c(3.1, 11.1, 4.1, 2))
barplot(mouse$Count, names.arg=mouse$Feature, horiz = TRUE, main = "Number of features in the mouse GRCm38 genome", las=1, xlim=c(0,80000) )

#2C Histograms
#generate histogram of two normal distribution
hist_values <- c(rnorm(10000), rnorm(10000)+4)
par(mar=c(3,3,3,1))
hist(hist_values, breaks = 30, xlim = c(-5,10))

#3A Providing color vectors
#read "male_female_counts"
gender_count <-read.table("bimm143_05_rstats/male_female_counts.txt", header=TRUE, sep="\t")
#barplot with rainbow color
par(mar=c(6,2,2,1))
barplot(height = gender_count$Count, names.arg = gender_count$Sample, col = rainbow(10), ylab = "Counts", ylim = c(0,20), las=2)

#barplot with 2 color
barplot(height = gender_count$Count, names.arg = gender_count$Sample, col = c("blue", "red"), ylab = "Counts", ylim = c(0,20), las=2)

#3B coloring by value
#read "up_down_expression"
gene <- read.table("bimm143_05_rstats/up_down_expression.txt", sep = "\t", header = TRUE)
#color down to blue, unchange to grey, up
palette(c("blue", "grey", "red"))
plot(gene$Condition1, gene$Condition2, col=gene$State)

#3C Dynamic use of color
#read meth file
meth <- read.table("bimm143_05_rstats/expression_methylation.txt", header=TRUE)
#generate denscolor
dcols <- densCols(meth$gene.meth, meth$expression)
plot(meth$gene.meth, meth$expression, col = dcols, pch=20)





