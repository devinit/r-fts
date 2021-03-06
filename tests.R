setwd("~/git/r-fts")
source("r-fts.R")

# Original documentation here: https://api.hpc.tools/docs/v1/

test.flow.group = fts.flow(
  boundary="year=2015",
  filterBy=c("destinationGlobalClusterCode:HEA","destinationLocationID:114,115"),
  groupBy="organization"
)
names(test.flow.group)
View(test.flow.group$report1$data)

# Completely different structure w/o group param
test.flow = fts.flow(
  boundary="year=2015&locationid=114",
  filterBy=c("destinationGlobalClusterCode:HEA")
)
names(test.flow)
View(test.flow)

# None of the RPM endpoints seem to work. They don't accept PUT or GET requests
# e.g.
content(GET("https://api.hpc.tools/v1/public/rpm/plan"))

test.cluster = fts.cluster()
names(test.cluster)

test.locat = fts.locat()
names(test.locat)

test.org = fts.org()
names(test.org)

test.proj.code = fts.proj.by.code("MM-15/H/75359/6491")
names(test.proj.code)

test.proj.id = fts.proj.by.id(1)
names(test.proj.id)

test.proj.plan.id = fts.proj.by.plan.id(34)
names(test.proj.plan.id)

test.proj.plan.code = fts.proj.by.plan.code("HSDN14")
names(test.proj.plan.code)

test.emergency.id = fts.emergency.id(611)
names(test.emergency.id)

test.emergency.year = fts.emergency.year(2013)
names(test.emergency.year)

test.emergency.iso3 = fts.emergency.iso3("SDN")
names(test.emergency.iso3)

test.plan.id = fts.plan.id(34)
names(test.plan.id)

test.plan.code = fts.plan.code("HAFG16")
names(test.plan.code)

test.plan.year = fts.plan.year(2013)
names(test.plan.year)

test.plan.iso3 = fts.plan.iso3("SDN")
names(test.plan.iso3)
