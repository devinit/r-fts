list.of.packages <- c("data.table","httr","reshape","progress")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("~/git/r-fts")

single_or_empty_fields = c(
  "id"
  ,"amountUSD"
  ,"budgetYear"
  ,"conributionType"
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

object_field_cols = c("type","id","name")

other_multi_fields = c("reportDetails", "keywords") # TODO: implement if necessary

flatten_flow = function(single_flow){
  flat_flow = data.frame(t(unlist(single_flow[single_or_empty_fields])),stringsAsFactors=F)
  
  for(i in 1:length(object_fields)){
    field_list = single_flow[object_fields[i]][[1]]
    if(length(field_list)>0){
      field_df = data.table(t(sapply(field_list,`[`,object_field_cols)))
      field_df$type = sapply(field_df$type,`[`,1)
      field_df_grouped = field_df[,.(id=paste(id,collapse=" | "),name=paste(name,collapse=" | ")),by=.(type)]
      field_prefix = names(object_fields)[i]
      flat_flow[,paste(field_prefix,field_df_grouped$type,"id",sep="_")] = field_df_grouped$id
      flat_flow[,paste(field_prefix,field_df_grouped$type,"name",sep="_")] = field_df_grouped$name
    }
  }
  
  return(flat_flow)
}

fts.flow = function(boundary=NULL,auth=NULL){
  page = 1
  base.url = paste0("https://api.hpc.tools/v1/public/fts/flow?",boundary)
  if(is.null(boundary)){
    stop("Boundary is a required field.")
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
    pb = progress_bar$new(total=max_page,format=" [:bar] :percent Elapsed: :elapsed; Remaining: :eta")
    data_list = list()
    flows = dat$data$flows
    flow_df = rbindlist(lapply(flows,flatten_flow),fill=T)
    data_list[[page]] = flow_df
    while("nextLink" %in% names(dat$meta)){
      page = page + 1
      pb$tick()
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

years = c(2000:2022)
boundary = paste0("year=",paste(years,collapse="%2C"))

flows = fts.flow(boundary,auth=auth)
fname = "all.csv"
fwrite(flows,fname)
