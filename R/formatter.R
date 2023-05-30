#' 项目状态格式化格式
#'
#' @param status 状态
#' @param id 项目内码
#' @param task 项目任务
#'
#' @return
#' @export
#'
#' @examples 返回值
#' projectFormatter()
projectFormatter <- function(id,task,status) {

res = paste0("
              <table>
    <tbody>\n
        <tr>
            <td colspan='3'><em>",id,":",task,"&nbsp;&nbsp;&nbsp;&nbsp;</em></td>
            <td><img src='",status,".png' width='31' height='20' alt='",status,"'>
        </tr>\n

        </tbody>
        </table>
             ")
return(res)

}
