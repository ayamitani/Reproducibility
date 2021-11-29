#-----------------------------------------------------------------
# Author: Aya Mitani
# Last updated: 2021-11-24
# What: Code to build models
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

# logistic regression models
m <- list()
m[[1]] <- glm(Pitbull ~ Puppy, data = bite2, family = binomial("logit"))
m[[2]] <- glm(Pitbull ~ Puppy + Male, data = bite2, family = binomial("logit"))
m[[3]] <- glm(Pitbull ~ Puppy + Male + Spayed, data = bite2, family = binomial("logit"))
msum <- lapply(m, summary)




