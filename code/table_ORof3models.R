#-----------------------------------------------------------------
# Author: Aya Mitani
# Last updated: 2021-11-24
# What: Create table of odds ratios from three logistic regression models
#----------------------------------------------------------------

source(here("code", "build_models.R"))

orvec <- list()
for(i in 1:3) orvec[[i]] <- OR_95CI(msum[[i]]$coef[,1], msum[[i]]$coef[,2], 0.05, 2)
orvec

varnames <- c("Intercept", "Puppy", "Male", "Spayed")
ORout <- data.frame(varnames, c(orvec[[1]], rep(NA, 2)), c(orvec[[2]], NA), orvec[[3]])
names(ORout) <- c("Variable", "M1", "M2", "M3")
ORtable <- xtable(ORout[-1,])
print.xtable(ORtable, file = here("results", "table_ORof3models.txt"), include.rownames = FALSE)
