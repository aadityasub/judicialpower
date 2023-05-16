## 
## Replication file 
##
## Judging the Post-Soviet Judiciaries: A Mixed Method Analysis of Judicial Power
##
## CFRS7
## 
## code.R
## 

library(psych)


data <- read.csv("~/Downloads/Replication/dataset.csv")

# Factor Analysis
fa.data <- data[, -c(1:4)]
KMO <- KMO(r=cor(fa.data))
corrtest <- cortest.bartlett(cor(fa.data), nrow(fa.data))
print(KMO)
print(corrtest)

eigenvalues <- eigen(cor(fa.data))
eigenvalues$values

fa <- fa(r=fa.data, nfactors = 3, min.err = 0.01, max.iter = 200, covar = FALSE,
         smc = TRUE, fm = "pa", rotate = "varimax")
fa$BIC
fa$TLI

f.scores <- factor.scores(fa.data, fa, method = "Bartlett")
data <- cbind(data, f.scores$scores)

# Creating LJPI
data$ljpi <- (data$PA1 * 0.36) + (data$PA2 * 0.31) + (data$PA3 * 0.33)

# Transforming variable for easier interpretation/graphs
data$ljpi <- data$ljpi + 2

## end of code