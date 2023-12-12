impute <- function(df)
{
  temps <- list()
  
  for (cat in c(0,1,2,3))
  {
    temp <- df[df["Category"]==cat,]
    means <- colMeans(temp, na.rm = T)
    
    for (col in colnames(temp))
    {
      mis_id <- which(is.na(temp[col]))
      if (length(mis_id) != 0)
      {
        temp[mis_id,col] <- means[col]
      }
    }
    temps[[as.character(cat)]] <- temp
  }
  return(temps)
}

hcv <- read.csv("data.csv")
temps <- impute(hcv)

hcv <- rbind(temps[['0']],temps[['1']],temps[['2']],temps[['3']])

par(mfrow=c(2,2))
for (i in c('0', '1', '2', '3'))
{
  boxplot(temps[[i]])
  title(switch(i, '0' = "Blood Donor", '1' = "Hepatitis", '2' = "Fibrosis", '3' = "Cirrhosis"))
}

bdescriptives <- summary(hcv)
norm_set_testing <- hcv[,-c(2,13)]

par(mfrow=c(3,4))

for (col in colnames(norm_set_testing))
{
  qqnorm(unlist(norm_set_testing[col]), main = col)
}

bens <- lapply(norm_set_testing, shapiro.test)

# H0 -> belong to normal
# Ha -> not H0

for (i in bens)
{
  print(i$p.value)
}

## None belong to the normal distribution
## all p-values are below the confidence interval alpha = 0.05

kruskal <- kruskal.test(norm_set_testing)
kruskal

library(conover.test)

conover.test(norm_set_testing)