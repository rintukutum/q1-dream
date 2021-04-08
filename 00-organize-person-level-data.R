rm(list=ls())
####
#### covert long format data to wide format data
#### prefer
#### person and time stamp based variable
#### 
q1_gs <- read.csv(
  "../data/q1-081920/training/goldstandard.csv",
  stringsAsFactors = FALSE
)
q1_y <- plyr::dlply(q1_gs,'status')

sapply(q1_y,nrow)
# 0      1 
# 44947  4944

q1y1 <- q1_y['1'][[1]]
q1y0 <- q1_y['0'][[1]]


q1_measure <- readr::read_csv(
  '../data/q1-081920/training/measurement.csv'
)
length(unique(q1_measure$measurement_concept_id))
#[1] 492
save(q1_measure,file='../data/q1trMeasure.RData')
# In case connectio breaks
# 
# load('../data/q1trMeasure.RData')

q1y1.measure <- q1_measure[q1_measure$person_id %in% q1y1$person_id,]
#length(unique(q1y1.measure$measurement_concept_id))
#[1] 492
q1y0.measure <- q1_measure[q1_measure$person_id %in% q1y0$person_id,]
#length(unique(q1y0.measure$measurement_concept_id))
#[1] 492


# convert to specific format
# measurement names and codes
# temporal person specific data


