% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{cloud_sync_modify}
\alias{cloud_sync_modify}
\title{同步修改数据}
\usage{
cloud_sync_modify(
  config = config_cloud,
  data_tableName = "rds_icitem_view3",
  input_tableName = "rds_icitem_view3_input",
  FInterID = "FItemID"
)
}
\arguments{
\item{config}{配置}

\item{data_tableName}{数据表}

\item{input_tableName}{缓存表}

\item{FInterID}{内码}
}
\value{
返回值
}
\description{
同步修改数据
}
\examples{
cloud_sync_modify()
}
