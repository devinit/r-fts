list.of.packages <- c("data.table","httr","jsonlite")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("~/git/r-fts")

base_url = "https://api.hpc.tools/v2/public/project/"

flows = fread("plan_flows.csv")
flows = subset(flows, Status %in% c("paid", "commitment") & !is.na(`Destination project ID`))
flows$value = as.numeric(flows$`Amount USD`)
flows_tab = flows[,.(value_usd=sum(value, na.rm=T)),by=.(`Destination project ID`)]
project_ids = unique(flows_tab$`Destination project ID`)

project_list = list()
project_index = 1
pb = txtProgressBar(max=length(project_ids),style=3)
for(i in 1:length(project_ids)){
  project_id = project_ids[i]
  setTxtProgressBar(pb,i)
  project_url = paste0(base_url, project_id)
  project_json = fromJSON(project_url, simplifyVector=F)
  project = project_json$data$projectVersion
  field_definitions = list()
  for(def in project$plans[[1]]$conditionFields){
    field_definitions[[as.character(def$id)]] = def
  }
  
  field_values = project$projectVersionPlans[[1]]$projectVersionFields
  for(field in field_values){
    def = field_definitions[[as.character(field$conditionFieldId)]]
    if(!is.null(def) & !is.null(field$value)){
      project_df = data.frame(
        "project_id" = project_id,
        "currently_requested_funds" = project$currentRequestedFunds,
        "plan_name" = project$plans[[1]]$planVersion$name,
        "plan_id" = project$plans[[1]]$planVersion$planId,
        "question" = def$name,
        "answer" = field$value
      )
      project_list[[project_index]] = project_df
      project_index = project_index + 1
    }
  }
}
close(pb)

project_fields = rbindlist(project_list)

setnames(flows_tab, "Destination project ID", "project_id")
project_fields = merge(project_fields, flows_tab, by="project_id")
fwrite(project_fields, "fields_and_flows.csv")