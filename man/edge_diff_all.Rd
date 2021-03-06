% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{edge_diff_all}
\alias{edge_diff_all}
\title{针对边数据进行处理，计算本地新增的数据}
\usage{
edge_diff_all(
  config = cfg_edge,
  diff_add_view_name = "t_icitem_view3_diff_add",
  diff_del_view_name = "t_icitem_view3_diff_del",
  diff_modify_view_name = "t_icitem_view3_diff_modify",
  target_view_name = "t_icitem_view3_diff"
)
}
\arguments{
\item{config}{配置 配置}

\item{diff_add_view_name}{新增视图}

\item{diff_del_view_name}{删除视图}

\item{diff_modify_view_name}{修改视图}

\item{target_view_name}{合并后视图}
}
\value{
返回数据框
}
\description{
针对边数据进行处理，计算本地新增的数据
}
\examples{
edge_diff_all()
}
