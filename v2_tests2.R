list.of.packages <- c("data.table","httr","jsonlite")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("~/git/r-fts")

base_url = "https://api.hpc.tools/v2/public/project/"

# year_url = paste0(base_url, "search?date=2020&limit=100")
# year_json = fromJSON(year_url)
# project_ids = year_json$data$results$id
# 
# pages = year_json$data$pagination$pages
# pb = txtProgressBar(max=pages,style=3)
# for(page_num in 2:pages){
#   setTxtProgressBar(pb, page_num)
#   year_url = paste0(base_url, "search?date=2020&limit=100&page=", page_num)
#   year_json = fromJSON(year_url)
#   project_ids = c(project_ids, year_json$data$results$id)
# }
# close(pb)
# save(project_ids,file="project_ids.RData")
load("project_ids.RData")

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
        "plan_name" = project$plans[[1]]$planVersion$name,
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
save(project_fields,file="project_fields.RData")
fwrite(project_fields,"project_fields.csv")

unique_qs = unique(project_fields$question)
unique_as = unique(project_fields$answer)
unique_qs[which(grepl("cash|voucher",unique_qs,ignore.case=T))]
