% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/schedule.R
\name{data_read}
\alias{data_read}
\title{查询排产数据}
\usage{
data_read(
  FToken = "7434C334-6E04-43C7-B103-634963BF78EF",
 
    file_name = "data-raw/棱星数据项目管理套表_W21_v1.3_胡立磊_20230520.xlsx",
  sheet = "data",
  type = "WBS"
)
}
\arguments{
\item{FToken}{增加口令}

\item{file_name}{文件名}

\item{sheet}{页签名}

\item{type}{增加业务类型}
}
\value{
返回值
}
\description{
查询排产数据
}
\examples{
data_read()
}
