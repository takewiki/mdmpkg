% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{mdm_meta}
\alias{mdm_meta}
\title{定义主数据的元数据}
\usage{
mdm_meta(
  data_table = "t_icitem",
  selector = c("FItemID", "FNumber", "FName", "FModel", "FChartNumber", "FErpClsID"),
  FInterID = "FItemID"
)
}
\arguments{
\item{data_table}{数据表}

\item{selector}{字段}

\item{FInterID}{关键字}
}
\value{
返回值
}
\description{
定义主数据的元数据
}
\examples{
mdm_meta()
}
