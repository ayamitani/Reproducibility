#-----------------------------------------------------------------
# Author: Aya Mitani
# Last updated: 2021-11-24
# What: Code to analyze dog bite data
#----------------------------------------------------------------


# read in data
bite <- read.csv(here("data", "dogbite.csv"), header = TRUE)
head(bite)

# Are bites from pit bull more likely to be by puppy, male, spayed dogs?
bite2 <- bite %>%
  mutate(Puppy = ifelse(Age < 4, 1, 0),
         Male = ifelse(Gender == "M", 1, 0),
         Spayed = ifelse(SpayNeuter == "true", 1, 0),
         Pitbull = ifelse(Breedclean == "Pit bull", 1, 0))
# logistic regression
glmbite <- glm(Pitbull ~ Puppy + Male + Spayed, data = bite2, family = binomial("logit"))
sumbite <- summary(glmbite)

# use kable to make markdown table
kable(sumbite$coefficients, digits = 3)

# use xtable to make latex table
xtable(glmbite)
table1 <- xtable(glmbite,
                 # title of the table
                 caption = "Results from logistic regression analysis",
                 # label for referencing 
                 label = "table:logreg",
                 # number of decimal places
                 digits = 2
) 
print.xtable(table1, file = here("results", "table1.txt"))

# html table
print.xtable(table1, type = "html", caption.placement = "top")


# I want to compare results from these three models

m1 <- glm(Pitbull ~ Puppy, data = bite2, family = binomial("logit"))
m2 <- glm(Pitbull ~ Puppy + Male, data = bite2, family = binomial("logit"))
m3 <- glm(Pitbull ~ Puppy + Male + Spayed, data = bite2, family = binomial("logit"))

screenreg(list(m1, m2, m3), ci.force = TRUE)


# I want to compute the OR (95% CI) for a table

summary(m1)$coefficients[,1]


OR_95CI <- function(coef, se, siglevel, roundto){
  q <- 1 - siglevel / 2
  OR <- exp(coef)
  ORlcl <- exp(coef - qnorm(q) * se)
  ORucl <- exp(coef + qnorm(q) * se)
  ORresult <- paste0(format(round(OR, roundto), nsmall=roundto), " (", format(round(ORlcl, roundto), nsmall=roundto), ", ", format(round(ORucl, roundto), nsmall=roundto), ")")
  return(ORresult)
}  

m <- list()
m[[1]] <- glm(Pitbull ~ Puppy, data = bite2, family = binomial("logit"))
m[[2]] <- glm(Pitbull ~ Puppy + Male, data = bite2, family = binomial("logit"))
m[[3]] <- glm(Pitbull ~ Puppy + Male + Spayed, data = bite2, family = binomial("logit"))
msum <- lapply(m, summary)
msum[[1]]$coef
msum[[1]]$coef[,1]


orvec <- list()
for(i in 1:3) orvec[[i]] <- OR_95CI(msum[[i]]$coef[,1], msum[[i]]$coef[,2], 0.05, 2)
orvec

varnames <- c("Intercept", "Puppy", "Male", "Spayed")
ORout <- data.frame(varnames, c(orvec[[1]], rep(NA, 2)), c(orvec[[2]], NA), orvec[[3]])
names(ORout) <- c("Variable", "M1", "M2", "M3")
ORtable <- xtable(ORout[-1,])
print(ORtable, include.rownames = FALSE)

