list.of.packages <- c("data.table","httr","jsonlite")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("~/git/r-fts")

base_url = "https://api.hpc.tools/v2/public/project/"

# year_url = paste0(base_url, "search?date=2022&limit=100")
# year_json = fromJSON(year_url)
# project_ids = year_json$data$results$id
# 
# pages = year_json$data$pagination$pages
# pb = txtProgressBar(max=pages,style=3)
# for(page_num in 2:pages){
#   setTxtProgressBar(pb, page_num)
#   year_url = paste0(base_url, "search?date=2022&limit=100&page=", page_num)
#   year_json = fromJSON(year_url)
#   project_ids = c(project_ids, year_json$data$results$id)
# }
# close(pb)
# save(project_ids,file="project_ids_2022.RData")
load("project_ids_2021.RData")
project_ids_2021 = project_ids
load("project_ids_2022.RData")
project_ids_2022 = project_ids

project_ids = rbind(
  data.frame(id=project_ids_2021, year=2021),
  data.frame(id=project_ids_2022, year=2022)
)

project_list = list()
project_index = 1
pb = txtProgressBar(max=nrow(project_ids),style=3)
for(i in 1:nrow(project_ids)){
  # for(i in 1:10){
  project_id = project_ids$id[i]
  project_year = project_ids$year[i]
  setTxtProgressBar(pb,i)
  project_url = paste0(base_url, project_id)
  project_json = fromJSON(project_url, simplifyVector=F)
  project = project_json$data$projectVersion
  global_clusters_json = project$globalClusters
  global_clusters = c()
  for(global_cluster in global_clusters_json){
    global_clusters = c(global_clusters, global_cluster$name)
  }
  global_clusters_string = paste0(global_clusters, collapse=" | ")
  organisation_json = project$organizations
  organisation_ids = c()
  organisation_names = c()
  for(organisation in organisation_json){
    organisation_ids = c(organisation_ids, organisation$id)
    organisation_names = c(organisation_names, organisation$name)
  }
  organisation_ids_string = paste0(organisation_ids, collapse=" | ")
  organisation_names_string = paste0(organisation_names, collapse=" | ")
  # message(project$projectVersionPlans[[1]]$workflowStatusOption$type)
  field_definitions = list()
  for(def in project$plans[[1]]$conditionFields){
    field_definitions[[as.character(def$id)]] = def
  }
  
  field_values = project$projectVersionPlans[[1]]$projectVersionFields
  if(length(field_values) == 0){
    project_df = data.frame(
      "project_id" = project_id,
      "project_year" = project_year,
      "currently_requested_funds" = project$currentRequestedFunds,
      "plan_id" = project$plans[[1]]$planVersion$id,
      "plan_name" = project$plans[[1]]$planVersion$name,
      "global_clusters" = global_clusters_string,
      "organisation_ids" = organisation_ids_string,
      "organisation_names" = organisation_names_string,
      "question" = "No field questions",
      "answer" = "No field answers"
    )
    project_list[[project_index]] = project_df
    project_index = project_index + 1
  }else{
    for(field in field_values){
      def = field_definitions[[as.character(field$conditionFieldId)]]
      if(!is.null(def) & !is.null(field$value)){
        project_df = data.frame(
          "project_id" = project_id,
          "project_year" = project_year,
          "currently_requested_funds" = project$currentRequestedFunds,
          "plan_id" = project$plans[[1]]$planVersion$id,
          "plan_name" = project$plans[[1]]$planVersion$name,
          "global_clusters" = global_clusters_string,
          "organisation_ids" = organisation_ids_string,
          "organisation_names" = organisation_names_string,
          "question" = def$name,
          "answer" = field$value
        )
        project_list[[project_index]] = project_df
        project_index = project_index + 1
      }
    }
  }
}
close(pb)

project_fields = rbindlist(project_list)
save(project_fields,file="project_fields_21_22_v2.RData")
fwrite(project_fields,"project_fields_21_22_v2.csv")

unique_project_df = project_fields[,c("project_id", "project_year", "plan_id", "plan_name", "currently_requested_funds")]
fwrite(unique_project_df, "unique_projects_21_22.csv")

unique_qs = unique(project_fields$question)
unique_as = unique(project_fields$answer)
unique_qs[which(grepl("cash|voucher",unique_qs,ignore.case=T))]
