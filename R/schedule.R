
#' 查询排产数据
#'
#' @param type 增加业务类型
#' @param FToken 增加口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dataDemo_check()
dataDemo_check <-function( FToken='7434C334-6E04-43C7-B103-634963BF78EF',type='WBS'){

  sql <- paste0("select FId  from rds_schedulerDemo where FClassName ='",type,"'")
  data = tsda::sql_select2(token = FToken,sql = sql)
  res = data$FId
  return(res)
}


#' 查询排产数据
#'
#' @param file_name 文件名
#' @param sheet 页签名
#' @param type 增加业务类型
#' @param FToken 增加口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' data_read()
data_read <-function( FToken='7434C334-6E04-43C7-B103-634963BF78EF',
                       file_name="data-raw/棱星数据项目管理套表_W21_v1.3_胡立磊_20230520.xlsx",
                       sheet='data',type='WBS'
){
  data <- readxl::read_excel(path = file_name,
                             sheet = sheet, col_types = c("text",
                                                          "text", "text", "text", "text", "date",
                                                          "date", "text", "numeric", "text",
                                                          "text"))
  #只过滤指定类型的数据
  data = data[data$className == type , ]
 return(data)



}



#' 上传数据
#'
#' @param FToken 口令
#' @param table_name 表名
#' @param file_name 文件名
#' @param sheet 页签名
#' @param type 类型
#'
#' @return 返回值
#' @export
#'
#' @examples
#' data_upload()
data_upload <-function(FToken='7434C334-6E04-43C7-B103-634963BF78EF',
                                table_name='rds_scheduler',
                                file_name="data-raw/棱星数据项目管理套表_W21_v1.3_胡立磊_20230520.xlsx",
                                sheet='data',type='WBS'
                                ){
  data <- data_read(file_name = file_name,sheet = sheet,type = type)
  demo1 = dataDemo_check(FToken = FToken,type = type)
  #删除其中的测试数据
  flag_id = data$id  %in% demo1
  count_id = sum(flag_id)
  #进行取反
  flag_id2 =  !flag_id
  #剔除了演示数据
  data = data[flag_id2, ]
  ncount = nrow(data)
  if(ncount >0){
    tsda::db_writeTable2(token =FToken,table_name = table_name,r_object = data,append = T )
    res <- TRUE
  }else{
    res <- FALSE
  }

  return(res)



}

#' 排产数据查询
#'
#' @param FToken 口令
#' @param status 状态
#'
#' @return 返回值
#' @export
#'
#' @examples
#' data_query()
data_query <- function(FToken='7434C334-6E04-43C7-B103-634963BF78EF',status='open') {

  sql = paste0("select * from rds_scheduler
where FStatus = '",status,"'")
  data = tsda::sql_select2(token = FToken,sql = sql
                           )
  ncount =nrow(data)
  if(ncount >0){
    col_names =c('id','task','status','type','title','start','end','group','editable','className','style')
    names(data) <- col_names
    data$editable[data$editable =='1'] <- TRUE
    data$editable[data$editable =='0'] <- FALSE
    data$content = projectFormatter(data$id,data$task,data$status)
    data$start <- tsdo::left_vect(as.character(data$start),19)
    data$end <- tsdo::left_vect(as.character(data$end),19)
    data = data[data$start != '', c('id','content','title','start','end','group','className')]
    #data$subGroup = data$group
    #data$group = data$className

  }else{
    data = data.frame(id='1',
                      task='棱星数据演示',
                      status='open',
                      type='box',
                      title='这是一条演示数据',
                      start='2023-01-01 09:00:00',
                      end ='2023-01-01 12:00:00',
                      group='胡立磊',
                      editable=FALSE,
                      className='demo',
                      style='color: red;',stringsAsFactors = FALSE
                      )
  }
  return(data)

}


#' 排产数据查询
#'
#' @param FToken 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' data_query()
data_overDue <- function(FToken='7434C334-6E04-43C7-B103-634963BF78EF') {

  sql = paste0("select * from rds_scheduler
where FStatus = 'open' and FEnd <=getdate()")
  data = tsda::sql_select2(token = FToken,sql = sql
  )
  ncount =nrow(data)
  if(ncount >0){
    col_names =c('id','task','status','type','title','start','end','group','editable','className','style')
    names(data) <- col_names
    data$editable[data$editable =='1'] <- TRUE
    data$editable[data$editable =='0'] <- FALSE
    data$content = projectFormatter(data$id,data$task,data$status)
    data$start <- tsdo::left_vect(as.character(data$start),19)
    data$end <- tsdo::left_vect(as.character(data$end),19)
    data = data[data$start != '', c('id','content','title','start','end','group','className')]
    #data$subGroup = data$group
    #data$group = data$className

  }else{
    data = data.frame(id='1',
                      task='棱星数据演示',
                      status='open',
                      type='box',
                      title='这是一条演示数据',
                      start='2023-01-01 09:00:00',
                      end ='2023-01-01 12:00:00',
                      group='胡立磊',
                      editable=FALSE,
                      className='demo',
                      style='color: red;',stringsAsFactors = FALSE
    )
  }
  return(data)

}
