setwd("~/git/r-fts")
source("r-fts.R")

test.flow.group = fts.flow(
  boundary="year=2015",
  filterBy=c("destinationGlobalClusterCode:HEA","destinationLocationID:114,115"),
  groupBy="organization"
)
names(test.flow.group)
View(test.flow.group$report1$data)

# Completely different structure w/o group param
test.flow = fts.flow(
  boundary="year=2015",
  filterBy=c("destinationGlobalClusterCode:HEA","destinationLocationID:114,115")
)
names(test.flow)
View(test.flow$incoming)
