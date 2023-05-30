#' 任务关闭
#'
#' @param FToken 口令
#' @param FId 任务内码
#' @param FStart 实际开始时间
#' @param FEnd 实际完成时间
#' @param FNote 完成情况
#'
#' @return 返回值
#' @export
#'
#' @examples
#' task_close()
task_close <- function(FToken='7434C334-6E04-43C7-B103-634963BF78EF',FId='A0027',FStart,
                       FEnd,
                       FNote) {

  sql = paste0("update a set  a.FStatus = 'close' from rds_scheduler a
where FId ='",FId,"' and FStatus ='open'")
res <-tsda::sql_update2(token = FToken,sql_str = sql)
#更新状态字段
sql_ins = paste0("insert into rds_schedulerStatus(FId,FStart,FEnd,FNote) values('",FId,"','",FStart,"','",FEnd,"','",FNote,"')")
tsda::sql_update2(token = FToken,sql_str = sql_ins)
return(res)

}
