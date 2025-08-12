#install.packages("pls")  # if not installed
library(pls)

# Example data structure
# your_data <- data.frame(year = ..., age1 = ..., age2 = ..., ..., ageN = ..., selectivity = ...)
df <- ebswp::read_rep(here::here("runs","lastyr","pm.rep") )
names(df )
head(df$T1 )
dfC<-df$C[14:61,]
dfC<-dfC/rowSums(dfC)
rowSums(dfC)

# Set formula (assumes selectivity is the response and all others are predictors)
pcr_model <- pcr(selectivity ~ ., data = dfC[, -1],  # drop 'year' if present
                 scale = TRUE, validation = "CV")
