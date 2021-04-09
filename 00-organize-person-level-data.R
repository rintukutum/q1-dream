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
# colnames(q1_measure)
# [1] "measurement_id"                "person_id"                    
# [3] "measurement_concept_id"        "measurement_date"             
# [5] "measurement_datetime"          "measurement_time"             
# [7] "measurement_type_concept_id"   "operator_concept_id"          
# [9] "value_as_number"               "value_as_concept_id"          
# [11] "unit_concept_id"               "range_low"                    
# [13] "range_high"                    "provider_id"                  
# [15] "visit_occurrence_id"           "visit_detail_id"              
# [17] "measurement_source_value"      "measurement_source_concept_id"
# [19] "unit_source_value"             "value_source_value"

# In case connection breaks
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

person.data.y1 <- plyr::dlply(q1y1.measure,'person_id')
person.data.y0 <- plyr::dlply(q1y0.measure,'person_id')

mzPerson <- rbind(
    data.frame(meauresPerPerson = sapply(person.data.y1,nrow),
          y='positive',stringsAsFactors = FALSE),
    data.frame(meauresPerPerson = sapply(person.data.y0,nrow),
          y='negative',stringsAsFactors = FALSE)
  )


library(ggplot2)
library(ggpubr)
p <- ggplot(mzPerson,aes(x=meauresPerPerson)) +
  geom_histogram() +
  facet_wrap(facets = "y") +
  theme_pubr()
dir.create('./figures',showWarnings = FALSE)

pdf('./figures/00-measurements-per-person.pdf',
    width = 5,height = 5)
p
dev.off()

# cat ./data/q1-081920/training/location.csv | awk '{split($0,a,","); print a[6]}' | uniq 
library(reshape2)
xtest <- person.data.y1[[2]]

xwide <- dcast(
  xtest,
  person_id~measurement_concept_id,
  value.var = "value_as_number",fun.aggregate = mean
)

pDF.y1 <- plyr::ldply(lapply(person.data.y1,function(x){
  dcast(
    x,
    person_id~measurement_concept_id,
    value.var = "value_as_number",fun.aggregate = mean
  )
}))

pDF.y0 <- plyr::ldply(lapply(person.data.y0,function(x){
  dcast(
    x,
    person_id~measurement_concept_id,
    value.var = "value_as_number",fun.aggregate = mean
  )
}))

save(pDF.y0,pDF.y1, file = '../data/pDF.measure.RData')













