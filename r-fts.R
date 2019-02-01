list.of.packages <- c("data.table","httr","reshape")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

flatten = function(x){
  return(data.frame(t(unlist(x)),stringsAsFactors=F))
}

fts.flow = function(boundary=NULL,filterBy=NULL,groupBy=NULL,auth=NULL){
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
    if("flows" %in% names(dat$data)){
      data_list = list(
        "incoming"=data.frame(dat$data$incoming,stringsAsFactors=F),
        "outgoing"=data.frame(dat$data$outgoing,stringsAsFactors=F),
        "internal"=data.frame(dat$data$internal,stringsAsFactors=F)
      )
      flows = dat$data$flows
      flow_df = rbindlist(lapply(flows,flatten),fill=T)
      data_list[["flows"]] = flow_df
      return(data_list)
    }else{
      data_list = list()
      for(name in names(dat$data)){
        report = rbindlist(lapply(dat$data[name],flatten),fill=T)
        ends_in_numeric = grepl("^[[:digit:]]+$",substr(names(report),nchar(names(report)),nchar(names(report))))
        if(sum(ends_in_numeric)>2){
          numeric_names = unique(gsub("[\\.[:digit:]]+$","",names(report)[ends_in_numeric]))
          names(report)[which(names(report) %in% numeric_names)] = paste0(names(report)[which(names(report) %in% numeric_names)],".0")
          ends_in_numeric = grepl("^[[:digit:]]+$",substr(names(report),nchar(names(report)),nchar(names(report))))
          report_meta = report[,!ends_in_numeric,with=F]
          report_data = report[,ends_in_numeric,with=F]
          report_data = melt(report_data,measure.vars=names(report_data))
          num_indexes = gregexpr("[\\.[:digit:]]+$",report_data$variable)
          report_data$index = unlist(regmatches(report_data$variable,num_indexes))
          report_data$index = substr(report_data$index,2,nchar(report_data$index))
          report_data$variable = gsub("[\\.[:digit:]]+$","",report_data$variable)
          report_data = dcast(report_data,index~variable)
          report = list("data"=report_data,"meta"=report_meta)
        }
        data_list[[name]] = report
      }
      return(data_list)
    }
    
  }else{
    stop("HTTP error: ",res$status_code)
  }
}

fts.cluster = function(auth=NULL){
  base.url = "https://api.hpc.tools/v1/public/global-cluster"
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
    if(length(dat$data)==0){
      data = data.frame()
    }else{
      data = rbindlist(lapply(lapply(dat$data,flatten),data.frame),fill=T)
    }
    return(data)
  }else{
    stop("HTTP error: ",res$status_code)
  }
}

fts.locat = function(auth=NULL){
  base.url = "https://api.hpc.tools/v1/public/location"
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
    if(length(dat$data)==0){
      data = data.frame()
    }else{
      data = rbindlist(lapply(lapply(dat$data,flatten),data.frame),fill=T)
    }
    return(data)
  }else{
    stop("HTTP error: ",res$status_code)
  }
}

fts.org = function(auth=NULL){
  base.url = "https://api.hpc.tools/v1/public/organization"
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
    if(length(dat$data)==0){
      data = data.frame()
    }else{
      data = rbindlist(lapply(lapply(dat$data,flatten),data.frame),fill=T)
    }
    return(data)
  }else{
    stop("HTTP error: ",res$status_code)
  }
}

fts.proj.by.code = function(code=NULL,auth=NULL){
  base.url = paste0("https://api.hpc.tools/v1/public/project/code/",code)
  if(is.null(code)){
    stop("Code is a required field.")
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
    if(length(dat$data)==0){
      data = data.frame()
    }else{
      data = data.frame(flatten(dat$data))
    }
    return(data)
  }else{
    stop("HTTP error: ",res$status_code)
  }
}

fts.proj.by.id = function(id=NULL,auth=NULL){
  base.url = paste0("https://api.hpc.tools/v1/public/project/id/",id)
  if(is.null(id)){
    stop("ID is a required field.")
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
    if(length(dat$data)==0){
      data = data.frame()
    }else{
      data = data.frame(flatten(dat$data))
    }
    return(data)
  }else{
    stop("HTTP error: ",res$status_code)
  }
}

fts.proj.by.plan.id = function(plan.id=NULL,auth=NULL){
  base.url = paste0("https://api.hpc.tools/v1/public/project/plan/",plan.id)
  if(is.null(plan.id)){
    stop("Plan ID is a required field.")
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
    if(length(dat$data)==0){
      data = data.frame()
    }else{
      data = rbindlist(lapply(lapply(dat$data,flatten),data.frame),fill=T)
    }
    return(data)
  }else{
    stop("HTTP error: ",res$status_code)
  }
}

fts.proj.by.plan.code = function(plan.code=NULL,auth=NULL){
  base.url = paste0("https://api.hpc.tools/v1/public/project/plancode/",plan.code)
  if(is.null(plan.code)){
    stop("Plan code is a required field.")
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
    if(length(dat$data)==0){
      data = data.frame()
    }else{
      data = rbindlist(lapply(lapply(dat$data,flatten),data.frame),fill=T)
    }
    return(data)
  }else{
    stop("HTTP error: ",res$status_code)
  }
}


