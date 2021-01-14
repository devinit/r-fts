list.of.packages <- c("data.table","httr","jsonlite")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

test_url = "https://api.hpc.tools/v2/public/project/search?planCodes=HSOM19"
test_json = fromJSON(test_url,flatten=T)
res = data.frame(test_json$data$results)
res_types = sapply(res, typeof)
list_types = res_types[which(res_types=="list")]
non_list_type_names = names(res_types[which(res_types!="list")])
res_flat = res[,non_list_type_names]
list_type_names = names(list_types)
list_df = res[,list_type_names]
# Replace null cells to avoid shifting gaps
for(list_type_name in list_type_names){
  null_cells = sapply(list_df[,list_type_name],is.null)
  sub_df_names = names(list_df[which(!null_cells)[1],list_type_name][[1]])
  null_replacement = data.frame(t(rep(NA,length(sub_df_names))))
  names(null_replacement) = sub_df_names
  null_replacement_list = list()
  null_replacement_list[[1]] = null_replacement
  for(null_cell_index in which(null_cells)){
    list_df[null_cell_index,list_type_name][[1]] = null_replacement_list
  }
}
list_type_name="locations"
rownum=1
# Collapse one to many relationships
for(list_type_name in list_type_names){
  col = list_df[,list_type_name]
  col_length_list = unlist(sapply(col,nrow))
  if(length(col_length_list)>0 & max(col_length_list)>1){
    for(rownum in 1:length(col)){
      sub_df = col[[rownum]]
      sub_df_list = list()
      sub_df_list[[1]] = data.frame(t(sapply(sub_df,paste,collapse="|")))
      list_df[rownum,list_type_name][[1]] = sub_df_list
    }
  }
}
bound_lists = sapply(list_df,rbindlist)
for(list_type_name in list_type_names[5]){
  bound_df = bound_lists[[list_type_name]]
  bound_df_types = sapply(bound_df, typeof)
  sub_list_types = bound_df_types[which(bound_df_types=="list")]
  sub_list_type_names = names(sub_list_types)
  bound_df[,sub_list_type_names] = NULL
  if(nrow(bound_df)>0){
    names(bound_df) = paste(list_type_name,names(bound_df),sep=".")
    res_flat = cbind(res_flat,bound_df)
  }
}

fwrite(res_flat,"FTS_projects_flat.csv")
