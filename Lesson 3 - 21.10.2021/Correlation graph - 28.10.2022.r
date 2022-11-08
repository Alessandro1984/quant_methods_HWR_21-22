library(corrplot)
library(wooldridge)

View(wage1)

cov(wage1$wage, wage1$educ)

cor(wage1$wage, wage1$educ)

cov(wage1$educ, wage1$exper)

cor(wage1$educ, wage1$exper)

data_W <- wage1[,1:4]

cov(data_W)

cor(data_W)

data_W_corr <- cor(data_W)

corrplot(data_W_corr, method="number", type="upper")
