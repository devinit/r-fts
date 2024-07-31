list.of.packages <- c("data.table","httr","reshape","progress")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("~/git/r-fts")

single_or_empty_fields = c(
  "id"
  ,"amountUSD"
  ,"budgetYear"
  ,"contributionType"
  ,"createdAt"
  ,"date"
  ,"decisionDate"
  ,"description"
  ,"grandBargainEarmarkingType"
  ,"exchangeRate"
  ,"firstReportedDate"
  ,"flowType"
  ,"newMoney"
  ,"originalAmount"
  ,"originalCurrency"
  ,"method"
  ,"parentFlowId"
  ,"status"
  ,"updatedAt"
  ,"versionId"
  ,"boundary"
  ,"onBoundary"
  ,"refCode"
)

object_fields = c(
  "source"="sourceObjects",
  "destination"="destinationObjects"
)

object_field_cols = c("type","id","name","behavior")

object_organization_lists = c("organizationTypes","organizationSubTypes","organizationLevels")
object_project_lists = c("code")

other_multi_fields = c("keywords", "childFlowIds")

flatten_flow = function(single_flow){
  flat_flow = data.frame(t(unlist(single_flow[single_or_empty_fields])),stringsAsFactors=F)
  
  for(i in 1:length(object_fields)){
    field_list = single_flow[object_fields[i]][[1]]
    if(length(field_list)>0){
      field_df = data.table(t(sapply(field_list,`[`,object_field_cols)))
      field_df$type = sapply(field_df$type,`[`,1)
      field_df_grouped = field_df[,.(
        id=paste(id,collapse=" | "),
        name=paste(name,collapse=" | "),
        behavior=paste(behavior,collapse=" | ")
      )
        ,by=.(type)]
      field_prefix = names(object_fields)[i]
      flat_flow[,paste(field_prefix,field_df_grouped$type,"id",sep="_")] = field_df_grouped$id
      flat_flow[,paste(field_prefix,field_df_grouped$type,"name",sep="_")] = field_df_grouped$name
      flat_flow[,paste(field_prefix,field_df_grouped$type,"behavior",sep="_")] = field_df_grouped$behavior

      org_type_indices = which(sapply(field_list,`[`, i="type") == "Organization")
      org_specific_df = data.table(t(sapply(field_list[org_type_indices], `[`, object_organization_lists)))
      for(org_var in object_organization_lists){
        if(org_var %in% names(org_specific_df)){
          org_specific_df[,org_var] = paste(unlist(org_specific_df[,org_var,with=F]),collapse=" | ")
          flat_flow[,paste(field_prefix,"Organization",org_var,sep="_")] = paste(org_specific_df[,org_var,with=F],collapse=" | ")
        }
      }

      project_type_indices = which(sapply(field_list,`[`, i="type") == "Project")
      project_specific_df = data.table(t(sapply(field_list[project_type_indices], `[`, object_project_lists)))
      for(project_var in object_project_lists){
        if(project_var %in% names(project_specific_df)){
          project_specific_df[,project_var] = paste(unlist(project_specific_df[,project_var,with=F]),collapse=" | ")
          flat_flow[,paste(field_prefix,"Project",project_var,sep="_")] = paste(project_specific_df[,project_var,with=F],collapse=" | ")
        }
      }

    }
  }
  
  for(field in other_multi_fields){
    flat_flow[,field] = paste(single_flow[[field]],collapse=" | ")
  }
  
  flat_flow["report_type"] = single_flow$reportDetails[[1]]$sourceType
  flat_flow["report_organisation"] = single_flow$reportDetails[[1]]$organization
  flat_flow["report_channel"] = single_flow$reportDetails[[1]]$reportChannel
  flat_flow["report_date"] = single_flow$reportDetails[[1]]$date
  
  return(flat_flow)
}

fts.flow = function(boundary=NULL,filterBy=NULL,groupBy=NULL,auth=NULL){
  page = 1
  base.url = paste0("https://api.hpc.tools/v1/public/fts/flow?",boundary)
  if(is.null(boundary)){
    stop("Boundary is a required field.")
  }
  if(!is.null(filterBy)){
    filterByParam = paste0("filterBy=",filterBy)
    filterByCollapse = paste(filterByParam,collapse="&")
    base.url = paste(base.url,filterByCollapse,sep="&")
  }
  if(!is.null(groupBy)){
    groupByParam = paste0("groupby=",groupBy)
    base.url = paste(base.url,groupByParam,sep="&")
  }
  if(!is.null(auth)){
    res = GET(
      base.url
      ,authenticate(auth$user, auth$pass)
    )
  }else{
    res = GET(base.url)
  }
  if(res$status_code==200){
    dat = content(res)
    rm(res)
    max_page = ceiling(dat$meta$count/200)
    pb = txtProgressBar(max=max_page, style=3)
    data_list = list()
    flows = dat$data$flows
    flow_df = rbindlist(lapply(flows,flatten_flow),fill=T)
    data_list[[page]] = flow_df
    while("nextLink" %in% names(dat$meta)){
      page = page + 1
      setTxtProgressBar(pb, page)
      if(!is.null(auth)){
        res = GET(
          dat$meta$nextLink
          ,authenticate(auth$user, auth$pass)
        )
      }else{
        res = GET(dat$meta$nextLink)
      }
      if(res$status_code==200){
        dat = content(res)
        rm(res)
        flows = dat$data$flows
        flow_df = rbindlist(lapply(flows,flatten_flow),fill=T)
        data_list[[page]] = flow_df
      }
    }
    close(pb)
    return(rbindlist(data_list,fill=T))
  }else{
    stop("HTTP error: ",res$status_code)
  }
}
args = commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  auth=NULL
}else{
  auth = list("user"=args[1],"pass"=args[2])
}

years = c(2000:2030)
boundary = paste0("year=",paste(years,collapse="%2C"))
filterBy = c("destinationYear:2021%2C2022%2C2023")

flows = fts.flow(boundary,filterBy=filterBy,auth=auth)
fname = "all_21-23.csv"
fwrite(flows,fname)
