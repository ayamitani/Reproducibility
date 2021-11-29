#-----------------------------------------------------------------
# Author: Aya Mitani
# Last updated: 2021-11-24
# What: Read in raw dog bite data, 
#         remove incidences with missing data, 
#         clean breed and age variables, 
#         write new clean data
#----------------------------------------------------------------

biteraw <- read.csv(here("data", "raw data", "DOHMH_Dog_Bite_Data.csv"), header = TRUE, na.strings=c(""))
head(biteraw)
bitecc <- biteraw[complete.cases(biteraw),] # remove observations with missing values
dim(bitecc)
sortedbreed <- data.frame(sort(table(biteraw$Breed), decreasing = TRUE)) # which dog breeds are common?

bite <- 
  bitecc %>% 
  # create indicator for most common breeds
  mutate(pitbull = str_detect(Breed, regex("pit", ignore_case = TRUE)),
         shihtzu = str_detect(Breed, regex("Shih Tzu", ignore_case = TRUE)),
         chihuahua = str_detect(Breed, regex("Chihuahua", ignore_case = TRUE)),
         germanshepherd = str_detect(Breed, regex("German Shepherd", ignore_case = TRUE)),
         terrier = (str_detect(Breed, regex(c("terrier|jack"), ignore_case = TRUE)) & !str_detect(Breed, regex(c("pit"), ignore_case = TRUE))),
         husky = str_detect(Breed, regex(c("husky"), ignore_case = TRUE)),
         retriever = str_detect(Breed, regex(c("retriever|lab|golden"), ignore_case = TRUE)),
         maltese = str_detect(Breed, regex(c("maltese"), ignore_case = TRUE)),
         poodle = str_detect(Breed, regex(c("poodle"), ignore_case = TRUE)),
         spaniel = str_detect(Breed, regex(c("spaniel"), ignore_case = TRUE)),
         shiba = str_detect(Breed, regex(c("shiba"), ignore_case = TRUE)),
         bulldog = (str_detect(Breed, regex(c("bull"), ignore_case = TRUE)) & !str_detect(Breed, regex(c("pit"), ignore_case = TRUE))),
         beagle = str_detect(Breed, regex(c("beagle"), ignore_case = TRUE)),
  ) %>%
  mutate_if(is.logical, as.numeric) %>% # convert to numeric
  mutate(mix = ifelse(rowSums(across(pitbull:beagle)) > 1, 1, 0)) %>% # any of them mixed?
  # create new variable Breedclean
  mutate(Breedclean = case_when(
    shihtzu == 1 ~ "Shih tzu",
    chihuahua == 1 ~ "Chihuahua",
    germanshepherd == 1 ~ "German shepherd",
    terrier == 1 ~ "Terrier",
    husky == 1 ~ "Husky",
    retriever == 1 ~ "Retriever",
    maltese == 1 ~ "Maltese",
    poodle == 1 ~ "Poodle",
    spaniel == 1 ~ "Spaniel",
    shiba == 1 ~ "Shiba",
    bulldog == 1 ~ "Bull dog",
    beagle == 1 ~ "Beagle",
    pitbull == 1 | mix == 1 ~ "Pit bull",
    )
  ) %>%
  mutate(Breedclean = replace_na(Breedclean, "Other"), 
         Year = str_sub(DateOfBite, -4, -1)) %>% # create year of bite
  # clean age variable
  mutate(Ageclean = ifelse(str_detect(Age, "M|m|W|w"), 0, Age)) %>%
  mutate(Ageclean = ifelse(str_detect(Ageclean, "Y|y"), as.numeric(str_extract(Ageclean, "[0-9]+")), Ageclean)) %>%
  filter(str_length(Ageclean) <= 2) %>%
  mutate(Agenum = as.numeric(Ageclean)) %>%
  select(UniqueID, Year, Breed, Breedclean, Age, Agenum, Gender, SpayNeuter, Borough, ZipCode)

write.csv(bite, here("data", "dogbite.csv"), row.names = FALSE) # output new data 
