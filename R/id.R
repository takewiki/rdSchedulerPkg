#' 获取默认值
#'
#' @param FToken 口令
#' @param type 类型
#'
#' @return 返回值
#' @export
#'
#' @examples
#' taskId_defautValue()
taskId_defautValue <-function(FToken='7434C334-6E04-43C7-B103-634963BF78EF',type='WBS'){

  sql <- paste0("  select  FDefaultValue    from rds_md_billNoRule   where FType ='",type,"'")
  data = tsda::sql_select2(token = FToken,sql = sql)
  ncount =nrow(data)
  if (ncount >0){
    res <- data$FDefaultValue
  }else{
    res <- NULL
  }
  return(res)

}

#' 获取前缀
#'
#' @param FToken 口令
#' @param type 类型
#'
#' @return 返回值
#' @export
#'
#' @examples
#' taskId_prefix()
taskId_prefix <-function(FToken='7434C334-6E04-43C7-B103-634963BF78EF',type='WBS'){

  sql <- paste0("     select   FPrefix    from rds_md_billNoRule  where FType ='",type,"'")
  data = tsda::sql_select2(token = FToken,sql = sql)
  ncount =nrow(data)
  if (ncount >0){
    res <- data$FPrefix
  }else{
    res <- NULL
  }
  return(res)

}


#' 获取前缀
#'
#' @param FToken 口令
#' @param type 类型
#'
#' @return 返回值
#' @export
#'
#' @examples
#' taskId_valueLen()
taskId_valueLen <-function(FToken='7434C334-6E04-43C7-B103-634963BF78EF',type='WBS'){

  sql <- paste0("     select   FValueLen    from rds_md_billNoRule  where FType ='",type,"'")
  data = tsda::sql_select2(token = FToken,sql = sql)
  ncount =nrow(data)
  if (ncount >0){
    res <- data$FValueLen
  }else{
    res <- NULL
  }
  return(res)

}


#' 获取单据代码的下一个值
#'
#' @param FToken 口令
#' @param type 类型
#'
#' @return 返回值
#' @export
#'
#' @examples
#' taskId_NextValue()
taskId_NextValue <-function(FToken='7434C334-6E04-43C7-B103-634963BF78EF',type='QA'){

  sql <- paste0("  select  max(FId) as FMaxId  from rds_scheduler
where   FClassName ='",type,"'")
  data = tsda::sql_select2(token = FToken,sql = sql)
  maxId = data$FMaxId
  #print(maxId)
  if (!is.na(maxId)){
    info <- data$FMaxId
    total_len = nchar(info)
    prefix = taskId_prefix(FToken = FToken,type = type)
    prefix_len = nchar(prefix)
    value_len = total_len-prefix_len
    current = tsdo::right(info,value_len)
    current_int = as.integer(current)
    next_value = current_int +1
    next_char = as.character(next_value)
    mid_len = taskId_valueLen(FToken = FToken,type = type)-nchar(next_char)
    mid_char = paste0(rep('0',mid_len),collapse = '')
    res = paste0(prefix,mid_char,next_char)





  }else{
    #设置默认值
    res <- taskId_defautValue(FToken = FToken,type = type)
  }
  return(res)

}
