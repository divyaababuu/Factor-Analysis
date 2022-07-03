brand.ratings <- read.csv("Data_Factor_Analysis.csv", stringsAsFactors = TRUE)
head(brand.ratings)
summary(brand.ratings)
str(brand.ratings)

#not converting the brand from char to numeric because we are finding the association between brand and other variables

#rescaling the data
brand.sc <- brand.ratings
brand.sc[,1:9] <- scale (brand.ratings[,1:9])
#we select all rows and first 9 columns as 10th column is a factor variable.
summary(brand.sc)
str(brand.sc)

#correlation
cor(brand.sc[,1:9])
#based on values, +values mean positively correlated and -value means negatively correlated

#plot it 
library(corrplot)
## corrplot 0.84 loaded
corrplot(cor(brand.sc[,1:9]))
corrplot(cor(brand.sc[,1:9]), order = "hclust")

#mean or aggregate
brand.mean <- aggregate(. ~ brand, data=brand.sc, mean)
##brand.mean1 <- aggregate(. ~ brand, data=brand.ratings, mean)
##brand.mean1
brand.mean

rownames(brand.mean) <- brand.mean[, 1]
# use brand for the row name
brand.mean <- brand.mean [, -1]
# remove the brand name column by not selecting the first column
# negative index is used to exclude the variable

install.packages("gplots")
library(gplots)
heatmap.2(as.matrix(brand.mean),main = "Brand attributes",
          trace = "none", key = FALSE, dend = "none")

#PCA
brand.pc<- princomp(brand.mean, cor = TRUE)
#we added "cor =TRUE" to use correlation-based one.
summary(brand.pc) ##components are combinations of variables

plot(brand.pc,type="l") # scree plot
loadings(brand.pc) # pc loadings
brand.pc$scores # the principal components

biplot(brand.pc, main = "Brand positioning")
brand.mean["c",] - brand.mean["e",]
colMeans(brand.mean[c("b","c","f","g"),]) - brand.mean["e",]

