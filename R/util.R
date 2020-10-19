#加载数据库区域-------
library(tsda)

#设置配置文件------
#边上的连接信息
cfg_edge <- conn_config(config_file = 'data-raw/conn_lc_test.R')
#云上的连接信息
cfg_cloud <- conn_config(config_file = 'data-raw/conn_rds.R')
#定义辅助函数-------

#' 检查对象是否存在
#'
#' @param config 连接
#' @param object_name 对象名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' check_object_exist()
check_object_exist <- function(config=cfg_edge,object_name='t_icitem') {
  conn <- conn_open(conn_config_info = config)
  sql <- paste0("select 1 from sys.objects where name ='",object_name,"'")
  res_view <- tsda::sql_select(conn = conn,sql_str = sql)
  ncount_view <- nrow(res_view)
  if(ncount_view >0){
    res <- TRUE

  }else{
    res <- FALSE

  }
  conn_close(conn)
  return(res)


}


#' 删除视图
#'
#' @param config 配置
#' @param view_name 视图名称
#'
#' @return 无返回值
#' @export
#'
#' @examples
#' view_drop()
view_drop <- function(config=cfg_edge,view_name='t_icitem_view') {
  conn <- conn_open(conn_config_info = config)
  sql_view_delete <- paste0("drop view ",view_name)
  try(tsda::sql_update(conn = conn,sql_str = sql_view_delete))
  conn_close(conn)

}

# ----完成视图的创建,具有很大的适用性
# 所有的视频直接用于列的筛选，不用于计算
#' 视图创建
#'
#' @param config 配置文件
#' @param table_name  来源表名
#' @param view_suffix 视图后缀
#' @param selector 字段列表
#'
#' @return 无返回值
#' @export
#'
#' @examples
#' view_create()
view_create <- function(config=cfg_edge,table_name='t_icitem',view_suffix='_view',
                        selector=c('FItemID','FName')) {
  conn <- conn_open(conn_config_info = config)
  view_name = paste0(table_name,view_suffix)
  field_str = paste0(selector,collapse = ",")
  #检查对象是否存在
  if(check_object_exist(config = config,object_name = view_name)){
    #已经存在视频,执行删除
    view_drop(config = config,view_name = view_name)

  }
  #否则统一创建视频即可
  sql_view_create <- paste0("create view ",view_name," as select ",field_str,"   from  ",table_name)
  tsda::sql_update(conn = conn,sql_str = sql_view_create)

  conn_close(conn)





}

#' 针对表数据进行视频化
#'
#' @param config 配置文件
#'
#' @return 返回值
#' @export
#'
#' @examples
#' view_tableInfo_init()
view_tableInfo_init <- function(config=cfg_edge) {
  conn <- conn_open(conn_config_info = config)

  if(check_object_exist(config = config,object_name = 'rds_tableInfo')){
    #视图存在,删除视图
    view_drop(config = config,view_name = 'rds_tableInfo')

  }

  sql_create <- paste0("create view rds_tableInfo as
select o.name as FTableName,c.name as FFieldName,
case t.name  when 'varchar' then 'varchar('+ ltrim(str(c.max_length)) +')'
 when 'nvarchar' then 'nvarchar('+ ltrim(str(c.max_length)) +')'
when 'decimal' then 'decimal('+     ltrim(str(c.precision)) +',' + ltrim(str(c.scale)) + ')'  else  t.name end   as FTypeName,
case c.is_nullable when 1 then 'null' else 'not null' end  as  FNullable
from  sys.all_columns  c
inner join  sys.objects  o
on c.object_id = o.object_id
left join sys.types  t
on c.user_type_id = t.user_type_id")
  try(tsda::sql_update(conn = conn,sql_str = sql_create))
  conn_close(conn)

}



#' 查询表的元数据数据
#'
#' @param config 配置
#' @param table_name 表名
#'
#' @return 字段列表
#' @export
#'
#' @examples
#' tableInfo_queryField()
tableInfo_queryField <- function(config=cfg_edge,table_name='t_icitem',field_selector=NA) {
  conn <- conn_open(conn_config_info = config)

  sql <- paste0("select * from rds_tableInfo
where FTableName='",table_name,"'")
  res <- tsda::sql_select(conn = conn,sql_str = sql)
  conn_close(conn)
  ncount <- nrow(res)
  if(ncount>0){
    if(is.na(field_selector[1])){
      res <- res
    }else{

      res <- res[res$FFieldName %in% field_selector, ]

    }

  }
  return(res)


}


#数据类型处理----------------
#' 数据类型,允许空值
#'
#' @param n 数量默认为1
#'
#' @return 返回值
#' @export
#'
#' @examples
#' type_null()
type_null <- function(n=1) {
  res <- rep(' null ',n)
  return(res)

}

#' 数据类型不允许为空
#'
#' @param n 位数
#'
#' @return 返回值
#' @export
#'
#' @examples
#' type_not_null()
type_not_null <- function(n=1) {
  res <- rep(' not null ',n)
  return(res)

}

#' 获取最大号
#'
#' @param cfg 配置文件
#'
#' @return 返回值
#' @export
#'
#' @examples
#' local_max_version()
local_icitem_max_version <- function(cfg=cfg_lc) {
  conn <- conn_open(conn_config_info = cfg)
  sql <- paste0("select isnull(max(FVersion),0) as FVersion  from t_icitem_log")
  data <- sql_select(conn,sql)
  res <- data$FVersion + 1
  conn_close(conn)
  return(res)
}


local_icitem_pushToImg <- function(cfg=cfg_lc) {
  conn <- conn_open(conn_config_info = cfg)
  sql_del <- paste0("truncate table t_icitem_img")
  tsda::sql_update(conn,sql_del)

  sql_ins <- paste0("insert into t_icitem_img
select FItemID,FNumber,FName,FModel ,FChartNumber,FErpClsID  from t_icitem")
  tsda::sql_update(conn,sql_ins)
  conn_close(conn)
}


local_icitem_pushToDel <- function(cfg=cfg_lc) {
  conn <- conn_open(conn_config_info = cfg)
  sql_del <- paste0("truncate table t_icitem_Del")
  tsda::sql_update(conn,sql_del)

  sql_ins <- paste0("insert into t_icitem_Del
select FItemID,FNumber,FName,FModel ,FChartNumber,FErpClsID  from t_icitem_img")
  tsda::sql_update(conn,sql_ins)
  conn_close(conn)
}


local_icitem_diff <- function(cfg=cfg_lc) {
  conn <- conn_open(conn_config_info = cfg)

  sql <- paste0("insert into t_icitem_log(FItemID,FNumber,FName,FModel ,FChartNumber,FErpClsID,FVersion)
select * ,dbo.func_icitem_max_ver() as FVersion  from rds_icitem_diff
")
  tsda::sql_update(conn,sql)
  conn_close(conn)

}


local_icitem_push <- function(cfg=cfg_lc,remote = cfg_rds){
  conn_local <- conn_open(conn_config_info = cfg)
  conn_remote <- conn_open(conn_config_info = remote)
  sql_sel <- paste0("select FItemID,FNumber,FName,FModel ,FChartNumber,FErpClsID  from t_icitem_log
where FIsDo = 0 ")
  data <- tsda::sql_select(conn_local,sql_sel)
  ncount =nrow(data)
  if(ncount >0){

    tsda::db_writeTable(conn = conn_remote,table_name = 'rds_icitem_input',r_object = data,append = T)
    #更新数据
    #1 删除已有数据
    sql_del <-paste0("delete from rds_icitem
where FItemID  in
(select FItemID from rds_icitem_input)
")
    tsda::sql_update(conn_remote,sql_del)
    #2插入现有数据
    sql_ins <- paste0("insert into rds_icitem
select * from  rds_icitem_input")
    tsda::sql_update(conn_remote,sql_ins)
    #3删除暂存数据
    sql_truncate <- paste0("truncate table rds_icitem_input")
    tsda::sql_update(conn_remote,sql_truncate)
    #设置数据处理已完成
    sql_done <- paste0("update t_icitem_log set FIsDo = 1
where FIsDo = 0 ")
    tsda::sql_update(conn_local,sql_done)



  }
}


local_icitem_sync_auto <- function(cfg=cfg_lc,remote = cfg_rds) {
  #计算差异
  local_icitem_diff(cfg = cfg)
  #推送数据
  local_icitem_push(cfg = cfg,remote = remote)
  #删除数据
  local_icitem_pushToDel(cfg = cfg)
  #删除镜像
  local_icitem_pushToImg(cfg = cfg)

}

