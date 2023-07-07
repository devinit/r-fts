list.of.packages <- c("data.table","httr","jsonlite")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("~/git/r-fts")

# plan_ids = c(
#   447, #Nigeria 2014
#   472, #Nigeria 2015
#   496, #Nigeria 2016
#   536, #Nigeria 2017
#   642, #Nigeria 2018
#   714, #Nigeria 2019
#   869, #Nigeria 2020
#   1032, #Nigeria Humanitarian Response Plan 2021
#   1062, #Nigeria Humanitarian Response Plan 2022
#   1110 #Nigeria Humanitarian Response Plan 2023
# )
# plan_names = c(
#   "Nigeria 2014",
#   "Nigeria 2015",
#   "Nigeria 2016",
#   "Nigeria 2017",
#   "Nigeria 2018",
#   "Nigeria 2019",
#   "Nigeria 2020",
#   "Nigeria Humanitarian Response Plan 2021",
#   "Nigeria Humanitarian Response Plan 2022",
#   "Nigeria Humanitarian Response Plan 2023"
# )
# plan_years = c(2014:2023)
# 
# plan_metadata = data.frame(plan_id=plan_ids, plan_name=plan_names, plan_year=plan_years)
# 
# project_ids_list = list()
# project_ids_index = 1
# 
# base_url = "https://api.hpc.tools/v1/public/project/plan/"
# 
# for(plan_id in plan_ids){
#   message(plan_id)
#   plan_url = paste0(base_url, plan_id)
#   plan_json = fromJSON(plan_url)$data
#   tmp = data.frame(plan_id, project_id=plan_json$id)
#   project_ids_list[[project_ids_index]] = tmp
#   project_ids_index = project_ids_index + 1
# }
# 
# project_ids = rbindlist(project_ids_list)
# project_ids = merge(plan_metadata, project_ids,by="plan_id")
# 
# save(project_ids,file="project_ids_nga.RData")
load("project_ids_nga.RData")

base_url = "https://api.hpc.tools/v2/public/project/"

project_list = list()
project_index = 1
location_list = list()
location_index = 1
pb = txtProgressBar(max=nrow(project_ids),style=3)
for(i in 1:nrow(project_ids)){
  # for(i in 1:10){
  Sys.sleep(0.5)
  project_id = project_ids$project_id[i]
  project_year = project_ids$plan_year[i]
  setTxtProgressBar(pb,i)
  project_url = paste0(base_url, project_id)
  project_json = fromJSON(project_url, simplifyVector=F)
  project = project_json$data$projectVersion
  locations = project$locations
  if(length(locations)>0){
    for(j in 1:length(locations)){
      location = locations[[j]]
      location_id = location$id
      location_name = location$name
      location_iso3 = location$iso3
      if(is.null(location_iso3)){
        location_iso3 = NA
      }
      location_pcode = location$pcode
      if(is.null(location_pcode)){
        location_pcode = NA
      }
      location_admin_level = location$adminLevel
      location_lat = location$latitude
      if(is.null(location_lat)){
        location_lat = NA
      }
      location_lon = location$longitude
      if(is.null(location_lon)){
        location_lon = NA
      }
      tmp = data.frame(
        project_id,
        project_year,
        location_id,
        location_name,
        location_iso3,
        location_pcode,
        location_admin_level,
        location_lat,
        location_lon
      )
      location_list[[location_index]] = tmp
      location_index = location_index + 1
    }
  }
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
save(project_fields,file="project_fields_nga.RData")
fwrite(project_fields,"project_fields_nga.csv")

unique_project_df = project_fields[,c("project_id", "project_year", "plan_id", "plan_name", "currently_requested_funds")]
fwrite(unique_project_df, "unique_projects_nga.csv")

location_dataset = rbindlist(location_list)
save(location_dataset, file="location_dataset_nga.RData")
fwrite(location_dataset, "location_dataset_nga.csv")
