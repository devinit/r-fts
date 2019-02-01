# r-fts
FTS API Plugin for R

### Authentication
If you have an FTS API username and password, create an `auth` list object like so:

```
auth = list(
  "user"="YourUserNameHere",
  "pass"="YourPasswordHere"
)
```

This object can then be passed to any of the R-FTS functions like so:

```
# Without named params
proj = fts.proj.by.id(1,auth)
# With named params
proj = fts.proj.by.id(id=1,auth=auth)
```

### Examples of use
```
# Returns report1, report2, report3, and report4 as lists
# Each list has attributes data, and meta
test.flow.group = fts.flow(
  boundary="year=2015",
  filterBy=c("destinationGlobalClusterCode:HEA","destinationLocationID:114,115"),
  groupBy="organization"
)

# Without groupBy, the structure changes quite dramatically
# This will return incoming, outgoing, internal, and flows
test.flow = fts.flow(
  boundary="year=2015",
  filterBy=c("destinationGlobalClusterCode:HEA","destinationLocationID:114,115")
)

test.cluster = fts.cluster()

test.locat = fts.locat()

test.org = fts.org()

test.proj.code = fts.proj.by.code("MM-15/H/75359/6491")

test.proj.id = fts.proj.by.id(1)

test.proj.plan.id = fts.proj.by.plan.id(34)

test.proj.plan.code = fts.proj.by.plan.code("HSDN14")

test.emergency.id = fts.emergency.id(611)

test.emergency.year = fts.emergency.year(2013)

test.emergency.iso3 = fts.emergency.iso3("SDN")

test.plan.id = fts.plan.id(34)

test.plan.code = fts.plan.code("HAFG16")
names(test.plan.code)

test.plan.year = fts.plan.year(2013)

test.plan.iso3 = fts.plan.iso3("SDN")
```

### Further documentation
Further documentation can be found at the source API here:

https://api.hpc.tools/docs/v1/

### Disclaimer
Development Initiatives Poverty Research, Ltd. does not own any of the data returned by this API. Data is owned by UNOCHA, and access to the data is determined through their terms and conditions.