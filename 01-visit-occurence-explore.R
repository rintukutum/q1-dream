rm(list=ls())
visitOcc <- readr::read_csv(
  "../data/q1-081920/training/visit_occurrence.csv"
)
q1_gs <- readr::read_csv(
  "../data/q1-081920/training/goldstandard.csv"
)
q1_y <- plyr::dlply(q1_gs,'status')

q1y1 <- q1_y['1'][[1]]
q1y0 <- q1_y['0'][[1]]

q1y1.visit <- visitOcc[visitOcc$person_id %in% q1y1$person_id,]
q1y0.visit <- visitOcc[visitOcc$person_id %in% q1y0$person_id,]

y1.person <- plyr::dlply(q1y1.visit,'person_id')
y0.person <- plyr::dlply(q1y0.visit,'person_id')

person.getYearFreq <- function(x){
  start.year <- as.numeric(format(x$visit_start_date,"%Y"))
  end.year <- as.numeric(format(x$visit_end_date,"%Y"))
  syDF <- data.frame(table(start.year),
             stringsAsFactors = FALSE)
  colnames(syDF)[1:2] <- c('year','freq')
  syDF$type.year <- 'start'
  eyDF <- data.frame(table(end.year),
             stringsAsFactors = FALSE)
  colnames(eyDF)[1:2] <- c('year','freq')
  eyDF$type.year <- 'end'
  return(rbind(syDF,eyDF))
}

y1.person.yf <- lapply(y1.person,person.getYearFreq)
y1.person.yfDF <- plyr::ldply(y1.person.yf, .id = 'person_id')
y1.person.yfDF$y <- 'positive'

y0.person.yf <- lapply(y0.person,person.getYearFreq)
y0.person.yfDF <- plyr::ldply(y0.person.yf, .id = 'person_id')
y0.person.yfDF$y <- 'negative'

person.yfDF <- rbind(y1.person.yfDF, y0.person.yfDF)

person.yfDF$year <- as.numeric(as.character(person.yfDF$year))

#' graph
#' person is a line
#' visit freq per year (y-axis)
#' year in x-axis
#' type(+ve,-ve) is color
#' facet start and end


library(ggplot2)
library(ggpubr)
library(hexbin)
pdat <- person.yfDF[person.yfDF$type.year=='start',]
p.start <- ggplot(pdat,aes(x=year,y=freq)) +
  geom_hex(bins = 30)+
  facet_wrap(facets = 'y',nrow = 2)

pdf('./figures/01-visit-per-year.pdf',
    width = 5,height = 5)  
p.start
dev.off()
    

# start and end (diff) vs total freq
getDIFF <- function(x){
  start.year <- min(x$year)
  end.year <- max(x$year)
  total.freq <- sum(x$freq)
  outdf <- data.frame(
    start.year=start.year,
    end.year = end.year,
    total.freq = total.freq
    )
  return(outdf)
}
pdiff <- plyr::dlply(
  pdat,'person_id',
  getDIFF
)
pdiffDF <- plyr::ldply(pdiff,.id = "person_id")
pdiffDF$person_id <- as.numeric(as.character(pdiffDF$person_id))
pdiffDF$y <- ifelse(
  pdiffDF$person_id %in% q1y1$person_id,
  yes = 'positive',no = "negative" )   
pbox <- ggplot(pdiffDF,aes(x=y,y=total.freq)) +
  geom_boxplot()
pdf('./figures/01-type-visit-freq.pdf',
    width = 5,height = 5)  
pbox + ylab('Total visit frequency')
dev.off()

pbox <- ggplot(pdiffDF,aes(x=start.year,y=total.freq)) +
  geom_boxplot()



summarize.pDIFF <- plyr::ddply(
  pdiffDF,"y",function(x){
    plyr::ddply(x,"start.year",function(xx){
      plyr::ddply(xx,"end.year",function(xxx){
        data.frame(
          start = xxx$start.year[1],
          end = xxx$end.year[1],
          total = sum(xxx$total)
        )
      })
    })
  })
ppoint <- ggplot(summarize.pDIFF,aes(x=start,y=end)) +
  geom_point(aes(size=log10(total),fill=log10(total)),
             shape=21,color="white") +
  facet_wrap(facets = 'y',nrow = 2)
pdf('./figures/01-summarize-start-end-total-visit.pdf',
    width = 8,height = 5)  
ppoint 
dev.off()


       