#0、加载数据库区域-------
library(tsda)

#1、置配置文件------
#1.1、边连接信息
cfg_edge <- conn_config(config_file = 'data-raw/conn_lc_test.R')
#1.2云连接信息
cfg_cloud <- conn_config(config_file = 'data-raw/conn_rds.R')

#2、数据类型处理----------------
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


#3、定义辅助函数-------


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

#' 检验代码防止误操作
#'
#' @param pwd 密码
#'
#' @return 返回值
#'
#' @examples
#' check_pwd()
check_pwd <- function(pwd) {
  if(pwd == 'rds@2020'){
    res <- TRUE
  }else{
    res<- FALSE
  }
  return(res)

}

#4、定义视图操作---------

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




# ----完成视图的创建,具有很大的适用性
# 所有的视频直接用于列的筛选，不用于计算
#' 视图创建
#'
#' @param config 配置文件
#' @param table_name  来源表名
#' @param selector 字段列表
#' @param view_name  视图名称
#'
#' @return 无返回值
#' @export
#'
#' @examples
#' view_create()
view_create <- function(config=cfg_edge,table_name='t_icitem',view_name='t_icitem_view',
                        selector=c('FItemID','FName')) {
  conn <- conn_open(conn_config_info = config)
  #view_name = paste0(table_name,view_suffix)
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

#' 进行表的选择
#'
#' @param view_name 视图名
#' @param config 配置
#'
#' @return 返回值
#' @export
#'
#' @examples
#' table_select()
view_select <- function(config=cfg_edge,view_name='t_icitem_view') {

  conn <- conn_open(conn_config_info = config)
  sql <- paste0("select * from ",view_name)
  res <- tsda::sql_select(conn=conn,sql_str = sql)
  return(res)
  conn_close(conn)




}

# 5、定义数据表的操作---------

#' 复制表结构的数据
#'
#' @param config 配置
#' @param src_table_name 数据源表名
#' @param target_table_name 目的表名
#' @param field_selector 字段选择器
#' @param pwd 密码
#'
#' @return 返回值
#' @export
#'
#' @examples
#' table_createFromCopy()
table_createFromCopy <- function(config=cfg_edge,
                                 src_table_name='t_icitem',
                                 target_table_name='t_icitem_demo',
                                 field_selector=NA,
                                 pwd='') {
  conn <- conn_open(conn_config_info = config)
  if(check_object_exist(config = config,object_name = target_table_name)){
    table_drop(config = config,table_name = target_table_name,pwd = pwd)
  }
  data <-tableInfo_queryField(config = config,
                              table_name = src_table_name,
                              field_selector = field_selector)
  sql_head <- paste0("create table ",target_table_name," (")
  sql_body <-paste(data$FFieldName,data$FTypeName,data$FNullable,sep = "  ",collapse = ' , ')
  sql_tail <- paste0(" )")
  sql_all <- paste0(sql_head,sql_body,sql_tail)
  try(tsda::sql_update(conn=conn,sql_str = sql_all))
  #print(sql_all)
  conn_close(conn)


}

#针对云边的数据库进行操作处理
#' 将边的数据结构复制到云表上
#'
#' @param config_edge 终的数据库配置
#' @param config_cloud 云的数据库配置
#' @param src_edge_tableName 复制来源，边的表名
#' @param target_cloud_tableName 目标数据表，云的数据
#' @param field_selector 字段选择器
#' @param pwd 控制码
#'
#' @return 返回值
#' @export
#'
#' @examples
#' table_createFromCopy_edgeToCloud()
table_createFromCopy_edgeToCloud<- function(config_edge = cfg_edge,
                                            config_cloud = cfg_cloud,
                                            src_edge_tableName ='t_icitem_view3_log' ,
                                            target_cloud_tableName ='rds_icitem_view3_log',
                                            field_selector=NA,
                                            pwd=''
                                            ) {
  #打开链接
  conn_edge <- conn_open(conn_config_info = config_edge)
  conn_cloud <- conn_open(conn_config_info = config_cloud)
  #删除表结构
  if(check_object_exist(config = config_cloud,object_name = target_cloud_tableName)){
    table_drop(config = config_cloud,table_name = target_cloud_tableName,pwd = pwd)
  }
  #构建SQL
  data <-tableInfo_queryField(config = config_edge,
                              table_name = src_edge_tableName,
                              field_selector = field_selector)
  sql_head <- paste0("create table ",target_cloud_tableName," (")
  sql_body <-paste(data$FFieldName,data$FTypeName,data$FNullable,sep = "  ",collapse = ' , ')
  sql_tail <- paste0(" )")
  sql_all <- paste0(sql_head,sql_body,sql_tail)
  print(sql_all)
  #执行SQL
  try(tsda::sql_update(conn=conn_cloud,sql_str = sql_all))

}




#' 删除表结构
#'
#' @param config 配置文件
#' @param table_name  表名
#'
#' @return 无返回值
#' @export
#'
#' @examples
#' table_drop()
table_drop <- function(config=cfg_edge,table_name='t_icitem_view',pwd='') {
   #针对密码进行二次验证
  if (check_pwd(pwd)){
    conn <- conn_open(conn_config_info = config)
    sql_table_delete <- paste0("drop table ",table_name)
    try(tsda::sql_update(conn = conn,sql_str = sql_table_delete))
    conn_close(conn)
  }



}


#' 进行表的选择
#'
#' @param config 配置
#' @param table_name 表或视图名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' table_select()
table_select <- function(config=cfg_edge,table_name='t_icitem_view') {

    conn <- conn_open(conn_config_info = config)
    sql <- paste0("select * from ",table_name)
    res <- tsda::sql_select(conn=conn,sql_str = sql)
    return(res)
    conn_close(conn)




}

#' 按状态查询表
#'
#' @param config 配置文件
#' @param table_name 表名
#' @param FStatus 状态字段名
#' @param FValue 状态值
#'
#' @return 返回值
#' @export
#'
#' @examples
#' table_selectBy_Status()
table_selectBy_Status <- function(config=cfg_edge,table_name='t_icitem_view',FStatus='FIsDo',FValue=0) {
  conn <- conn_open(conn_config_info = config)
  sql <- paste0("select * from ",table_name," where ",FStatus,"  = ",FValue)
  print(sql)
  res <- tsda::sql_select(conn=conn,sql_str = sql)
  return(res)
  conn_close(conn)


}

#' 字段列表选择器
#'
#' @param config 配置
#' @param table_name 表名
#' @param exclude 不需要字段名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' table_fieldList()
table_fieldList <- function(config=cfg_edge,table_name='t_icitem',exclude='') {
  conn <- conn_open(conn_config_info = config)
  sql <- paste0("select FFieldName from rds_tableInfo where ftablename ='",table_name,"'")
  r <- tsda::sql_select(conn,sql)
  res <- r$FFieldName
  flag <- !res %in% exclude
  res <- res[flag]
  res <- paste0(res,collapse = ' , ')
  return(res)
  conn_close(conn)
  }


#' 更新表的状态整数型
#'
#' @param config 配置文件
#' @param table_name 表名
#' @param FStatus 状态字段
#' @param FFromValue 原值
#' @param FToValue  更改值
#'
#' @return 返回值
#' @export
#'
#' @examples
#' table_setBy_Status()
table_setBy_Status <- function(config=cfg_edge,
                               table_name='t_icitem_view',
                               FStatus='FIsDo',
                               FFromValue=0,
                               FToValue=1) {
  conn <- conn_open(conn_config_info = config)
  sql <- paste0("update  ",table_name," set ",FStatus," = ",FToValue," where ",FStatus," =  ",FFromValue)
  print(sql)
  tsda::sql_update(conn=conn,sql_str = sql)
  #return(res)
  conn_close(conn)


}






#' 查询表的元数据数据
#'
#' @param config 配置
#' @param table_name 表名
#'
#' @return 字段列表
#' @import tsda
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





#' 获取最大号
#'
#' @param cfg 配置文件
#'
#' @return 返回值
#' @export
#'
#' @examples
#' table_maxId()
table_maxId <- function(config=cfg_edge,table_name='t_icitem_log',field_name='FVersion') {
  conn <- conn_open(conn_config_info = config)
  sql <- paste0("select isnull(max(",field_name,"),0) as  ",field_name,"  from  ",table_name)
  data <- sql_select(conn,sql)
  res <- data[1,field_name]
  conn_close(conn)
  return(res)
}

#' 针对表进行清空拉个口子要见血
#'
#' @param config 配置
#' @param table_name 表名
#' @param pwd 密码
#'
#' @return 无返回值
#' @export
#'
#' @examples table_truncate()
table_truncate <- function(config=cfg_edge,table_name='t_dataTest',pwd='') {
  conn <- conn_open(conn_config_info = config)
  sql_del <- paste0("truncate table ",table_name)
  if(check_pwd(pwd)){
    try(tsda::sql_update(conn,sql_del))
  }
  #手工关闭链接
  conn_close(conn)



}




# 6、定义函数操作-------

#' 删除函数对象
#'
#' @param config 配置
#' @param func_name 函数名
#'
#' @return 无返回值
#' @export
#'
#' @examples
#' func_drop()
func_drop <- function(config=cfg_edge,func_name='func_icitem_max_ver') {
  conn <- conn_open(conn_config_info = config)
  sql_func_delete <- paste0("drop function ",func_name)
  try(tsda::sql_update(conn = conn,sql_str = sql_func_delete))
  conn_close(conn)

}


#' 返回日志文件的最大版本
#'
#' @param config 配置
#' @param table_name 表名
#' @param return_field 返回值字值
#' @param func_name 处理函数默认最大值
#' @param stat_func 统计函数默认为max
#' @param step 步骤默认为1也可以是-1
#' @param return_type 返回值类型默认为int或者float
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dbo_func_statField()
dbo_func_statField <- function(config=cfg_edge,
                                table_name='t_icitem_log',
                                return_field='FVersion',
                                return_type='int',
                                stat_func='max',
                                step=1,

                                func_name='func_icitem_max_ver') {
  conn <- conn_open(conn_config_info = config)
  #判断是否存在
  if(check_object_exist(config = config,object_name = func_name)){
    func_drop(config = config,func_name = func_name)
  }
  #创建函数
  sql <- paste0("CREATE    FUNCTION ",func_name,
  "() RETURNS  ",return_type,"  as
BEGIN
declare @ver ",return_type,"
select @ver = isnull(",stat_func,"(",return_field,"),0)   from ",table_name,"
RETURN  @ver+  ",step," end")

 print(sql)
 tsda::sql_update(conn = conn,sql_str = sql)
 conn_close(conn)





}





# 7、边数据-------
# 7.1、边数据定义-----
#' 针对边数据进行定义
#'
#' @param config 边配置
#' @param table_name 表名
#' @param view_suffix 后缀默认为_view
#' @param selector 选择器
#'
#' @return 返回值
#' @export
#'
#' @examples
#' edge_data_view()
edge_data_view <- function(config=cfg_edge,table_name='t_icitem',
                           view_name='t_icitem_view',
                           selector=c('FItemID','FName')) {
  view_create(config = config,table_name = table_name,view_name=view_name,selector = selector)

}


#' 增加边的镜像文件
#'
#' @param config 配置
#' @param src_view_name 数据源
#' @param target_table_name 镜像表名
#' @param pwd 控制码
#'
#' @return 返回值
#' @export
#'
#' @examples
#' edge_data_img()
edge_data_img <- function(config=cfg_edge,
                          src_view_name='t_icitem_view3',
                          target_table_name='t_icitem_img',
                          pwd='') {
  table_createFromCopy(config = config,
                       src_table_name = src_view_name,
                       target_table_name = target_table_name,
                       field_selector = NA,pwd = pwd
                        )

}
#' 边数据删除备份表
#'
#' @param config 配置
#' @param src_view_name 源数据
#' @param target_table_name 删除备份表
#' @param pwd 控制码
#'
#' @return 返回值
#' @export
#'
#' @examples
#' edge_data_del()
edge_data_del <- function(config=cfg_edge,
                          src_view_name='t_icitem_view3',
                          target_table_name='t_icitem_del',
                          pwd='') {
  table_createFromCopy(config = config,
                       src_table_name = src_view_name,
                       target_table_name = target_table_name,
                       field_selector = NA,pwd = pwd
  )

}

#' 创建表的日志
#'
#' @param config 配置
#' @param src_table_name 源表
#' @param target_table_name 日志表
#' @param field_selector 字段选择器
#' @param pwd 控制码
#'
#' @return 返顺值
#' @export
#'
#' @examples
#' edge_Log_create()
edge_Log_create <- function(config=cfg_edge,
                                src_table_name='t_icitem_view3_diff',
                                target_table_name='t_icitem_view3_log',
                                field_selector=NA,
                                pwd='') {
  conn <- conn_open(conn_config_info = config)
  if(check_object_exist(config = config,object_name = target_table_name)){
    table_drop(config = config,table_name = target_table_name,pwd = pwd)
  }
  data <-tableInfo_queryField(config = config,
                              table_name = src_table_name,
                              field_selector = field_selector)
  sql_head <- paste0("create table ",target_table_name," (")
  sql_body <-paste(data$FFieldName,data$FTypeName,data$FNullable,sep = "  ",collapse = ' , ')
  sql_log <-paste0(" , [FVersion] [int] NULL,
	[FIsDo] [int] NULL default 0,
	[FUpdateTime] [datetime] NULL default getdate() ")
  sql_tail <- paste0(" )")
  sql_all <- paste0(sql_head,sql_body,sql_log,sql_tail)
  print(sql_all)
  try(tsda::sql_update(conn=conn,sql_str = sql_all))
  #print(sql_all)
  conn_close(conn)

}

# 7.1.1差异处理使用视图---------

#' 差异新增
#'
#' @param config 配置
#' @param src_view_name 数据源
#' @param img_table_name 镜像数据
#' @param key_col 主键
#' @param target_view_name  目标视图
#'
#' @return 返回键
#' @export
#'
#' @examples
#' edge_diff_add()
edge_diff_add<- function(config=cfg_edge,
                         src_view_name="t_icitem_view3",
                         img_table_name="t_icitem_view3_img",
                         target_view_name='t_icitem_view3_diff_add',
                         key_col ='FItemId') {

  conn <- conn_open(conn_config_info = config)
  if(check_object_exist(config = config,object_name = target_view_name)){
    view_drop(config = config,view_name = target_view_name)
  }
  sql <- paste0("create view  ",target_view_name," as select * ,'add' as FDiffAction from  ",src_view_name," where  ",key_col,"  not in (select ",key_col," from  ",img_table_name,")")
  print(sql)
  tsda::sql_update(conn = conn,sql_str = sql)
  conn_close(conn)
  #return(res)


}

#' 检测已经修改的数据
#'
#' @param config 配置
#' @param src_view_name 视图数据源
#' @param img_table_name 镜像文件
#' @param key_col 主键
#' @param target_view_name  目标视图
#'
#' @return 返顺值
#' @export
#'
#' @examples
#' edge_diff_modify()
edge_diff_modify<- function(config=cfg_edge,
                            src_view_name="t_icitem_view3",
                            img_table_name="t_icitem_view3_img",
                            target_view_name='t_icitem_view3_diff_modify',

                            key_col ='FItemId') {

  conn <- conn_open(conn_config_info = config)
  if(check_object_exist(config = config,object_name = target_view_name)){
    view_drop(config = config,view_name = target_view_name)
  }
  sql <- paste0("create view ",target_view_name," as select * ,'modify' as FDiffAction from (select *   from  ",src_view_name," where  ",key_col,"   in (select ",key_col,"  from  ",img_table_name,") except select * from  ",img_table_name," ) a")
  print(sql)
  tsda::sql_update(conn = conn,sql_str = sql)
  conn_close(conn)
  #return(res)


}

#' 测试删除数据
#'
#' @param config 配置
#' @param src_view_name 视图
#' @param img_table_name 镜像文件
#' @param key_col 关键字段
#' @param target_view_name  目标视图
#'
#' @return 返回值
#' @import tsda
#' @export
#'
#' @examples
#' edge_diff_del()
edge_diff_del<- function(config=cfg_edge,
                         src_view_name="t_icitem_view3",
                         img_table_name="t_icitem_view3_img",
                         target_view_name='t_icitem_view3_diff_del',
                         key_col ='FItemId') {

  conn <- conn_open(conn_config_info = config)
  if(check_object_exist(config = config,object_name = target_view_name)){
    view_drop(config = config,view_name = target_view_name)
  }
  sql <- paste0("create view ",target_view_name," as select * , 'delete' as FDiffAction  from  ",img_table_name," where  ",key_col,"  not in (select ",key_col," from  ",src_view_name,")")
  print(sql)
  tsda::sql_update(conn = conn,sql_str = sql)
  conn_close(conn)
  #return(res)


}

#' 针对边数据进行处理，计算本地新增的数据
#'
#' @param diff_add_view_name 新增视图
#' @param diff_del_view_name  删除视图
#' @param diff_modify_view_name  修改视图
#' @param target_view_name  合并后视图
#' @param config 配置 配置
#'
#' @return 返回数据框
#' @export
#'
#' @examples
#' edge_diff_all()
edge_diff_all<- function(config=cfg_edge,
                     diff_add_view_name='t_icitem_view3_diff_add',
                     diff_del_view_name='t_icitem_view3_diff_del',
                     diff_modify_view_name='t_icitem_view3_diff_modify',
                     target_view_name='t_icitem_view3_diff') {
  conn <- conn_open(conn_config_info = config)
  if(check_object_exist(config = config,object_name = target_view_name)){
    view_drop(config = config,view_name = target_view_name)
  }
  sql <- paste0("create view ",target_view_name,
                " as select *  from ",diff_add_view_name,
                " union select *  from ",diff_del_view_name,
                " union select *  from ",diff_modify_view_name)
  print(sql)
  tsda::sql_update(conn = conn,sql_str = sql)
  conn_close(conn)





}

# 7.2、边数据操作------
#' 将视图数据推入静像文件
#'
#' @param config 配置
#' @param src_view_name 视图
#' @param img_table_name 静像文件
#' @param pwd 密码
#'
#' @return
#' @import tsda
#' @export
#'
#' @examples
#' edge_view_pushToImg()
edge_view_pushToImg <- function(config=cfg_edge,src_view_name='t_icitem_view3',img_table_name='t_icitem_view3_img',pwd='') {
  conn <- conn_open(conn_config_info = config)
  #删除数据
  table_truncate(config = config,table_name = img_table_name,pwd = pwd)

  sql_ins <- paste0("insert into ",img_table_name,"  select  *  from ",src_view_name)
  print(sql_ins)
  tsda::sql_update(conn,sql_ins)
  conn_close(conn)
}

#将静像文件删除进入备份Del表
#' 对静像文件进行备份
#'
#' @param config 配置
#' @param img_table_name 镜像文件
#' @param imgDel_table_name 备份文件
#' @param pwd 控制码
#'
#' @return
#' @import tsda
#' @export
#'
#' @examples
#' edge_img_pushToDel()
edge_img_pushToDel <- function(config=cfg_edge,img_table_name,imgDel_table_name,pwd='') {
  view_pushToImg(config = config,src_view_name = img_table_name,img_table_name = imgDel_table_name,pwd = pwd)
}



















#' 获取日志的最大版本号
#'
#' @param config 配置
#' @param log_table_name 日志名
#' @param func_name 函数名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' edge_log_maxVersion()
edge_log_maxVersion <- function(config=cfg_edge,
                                log_table_name='t_icitem_view3_log',
                                func_name='func_icitem_max_ver') {
  conn <- conn_open(conn_config_info = config)
  dbo_func_statField(config = config,table_name = log_table_name,
                     return_field = 'FVersion',return_type = 'int',
                     stat_func = 'max',
                     step =1,
                     func_name =  func_name)
  conn_close(conn)

}

#' 写入日志文件
#'
#' @param config 配置
#' @param log_table_name 日志表
#' @param func_name  版本
#' @param edge_diff_view_name
#'
#' @return
#' @export
#'
#' @examples
#' edge_diff_pushLog()
edge_diff_pushToLog <- function(config=cfg_edge,
                               log_table_name='t_icitem_view3_log',
                               func_name='func_icitem_max_ver',
                               edge_diff_view_name='t_icitem_view3_diff') {
  conn <- conn_open(conn_config_info = config)
  data_meta <- tableInfo_queryField(config = config,table_name = log_table_name)
  field_name <- data_meta$FFieldName
  idx <- ! field_name %in% c('FIsDo','FUpdateTime')
  field_name_sel <- field_name[idx]
  field_name_sql <- paste(field_name_sel,collapse = ' , ')


  sql <- paste0("insert into ",log_table_name,"( ", field_name_sql ," )
select * ,dbo.",func_name,"() as FVersion  from ",edge_diff_view_name)
  print(sql)
  tsda::sql_update(conn,sql)
  conn_close(conn)

}

#' 边向云端写入数据
#'
#' @param config_edge 边连接
#' @param config_cloud 云连接
#' @param input_table_name 表名
#' @param edge_diff_view_name 差异表
#'
#' @return 返回值
#' @export
#'
#' @examples
#' edge_log_pushToCloud_input()
edge_log_pushToCloud_input <- function(config_edge=cfg_edge,
                                       config_cloud=cfg_cloud,
                                       input_table_name='rds_icitem_view3_input',
                                       edge_diff_view_name='t_icitem_view3_diff') {
  #连接
  conn_edge <- conn_open(conn_config_info = config_edge)
  conn_cloud <- conn_open(conn_config_info = config_cloud)
  #数据
  data <- view_select(config = config_edge,view_name = edge_diff_view_name)
  ncount =nrow(data)
  if(ncount >0){
    tsda::db_writeTable(conn = conn_cloud,table_name = input_table_name,r_object = data,append = T)
  }



#待定义
}


#' 边向云端写入日志
#'
#' @param config_edge 边配置
#' @param config_cloud 云配置
#' @param cloud_log_table_name 日志文件
#' @param edge_log_table_name  日志文件
#'
#' @return 返回值
#' @export
#'
#' @examples
#' edge_log_pushToCloud_log()
edge_log_pushToCloud_log <- function(config_edge=cfg_edge,
                                     config_cloud=cfg_cloud,
                                     cloud_log_table_name='rds_icitem_view3_log',
                                     edge_log_table_name='t_icitem_view3_log') {
  #待定义
  #连接
  conn_edge <- conn_open(conn_config_info = config_edge)
  conn_cloud <- conn_open(conn_config_info = config_cloud)
  #数据

  data <- table_selectBy_Status(config = config_edge,table_name = edge_log_table_name)

  ncount =nrow(data)
  if(ncount >0){
    tsda::db_writeTable(conn = conn_cloud,table_name = cloud_log_table_name,r_object = data,append = T)
  }
  #针对日志进行记录处理
   table_setBy_Status(config = config_edge,table_name =edge_log_table_name)
}





# 8、云数据------
# 8.1、云数据定义----
#' 添加对云边数据库的初始化
#'
#' @param config 云数据库配置
#'
#' @return 返回值
#' @export
#'
#' @examples
#' cloud_tableInfo_init()
cloud_tableInfo_init <- function(config=cfg_cloud) {
  view_tableInfo_init(config = config)

}

#' 在云边创建表结果
#'
#' @param config_edge 边配置文件
#' @param config_cloud 云配置文件
#' @param src_edge_viewName 边视图文件
#' @param target_cloud_tableName 云表文件
#' @param field_selector 字段选择器
#' @param pwd 控制码
#'
#' @return 返回值
#' @export
#'
#' @examples
#' cloud_createTable_data()
cloud_createTable_data <- function(config_edge = cfg_edge,
                                    config_cloud = cfg_cloud,
                                    src_edge_viewName ='t_icitem_view3' ,
                                    target_cloud_tableName ='rds_icitem_view3',
                                    field_selector=NA,
                                    pwd='') {
  table_createFromCopy_edgeToCloud(config_edge = config_edge,
                                   config_cloud = config_cloud ,
                                   src_edge_tableName = src_edge_viewName,
                                   target_cloud_tableName = target_cloud_tableName,
                                   field_selector = field_selector,pwd = pwd)
}



#' 创建日志日志输入的文件
#'
#' @param config_edge 边配置文件
#' @param config_cloud 云配置文件
#' @param src_edge_diff_tableName 边差异表包含Action
#' @param target_cloud_input_tableName 云配置表
#' @param field_selector 字段选择器
#' @param pwd 控制码
#'
#' @return 返回值
#' @export
#'
#' @examples
#' cloud_createTable_input()
cloud_createTable_input <- function(config_edge = cfg_edge,
                                    config_cloud = cfg_cloud,
                                    src_edge_diff_tableName ='t_icitem_view3_diff' ,
                                    target_cloud_input_tableName ='rds_icitem_view3_input',
                                    field_selector=NA,
                                    pwd='') {
  table_createFromCopy_edgeToCloud(config_edge = config_edge,
                                   config_cloud = config_cloud ,
                                   src_edge_tableName = src_edge_diff_tableName,
                                   target_cloud_tableName = target_cloud_input_tableName,
                                   field_selector = field_selector,pwd = pwd)
}

#' 创建云边日志文件
#'
#' @param config_edge 边配置文件
#' @param config_cloud 云配置文件
#' @param src_edge_log_tableName   边日志表
#' @param target_cloud_log_tableName  云日志表
#' @param field_selector 字段选择器
#' @param pwd 控制码
#'
#' @return 返回值
#' @export
#'
#' @examples
#' cloud_createTable_log()
cloud_createTable_log <- function(config_edge = cfg_edge,
                                    config_cloud = cfg_cloud,
                                    src_edge_log_tableName ='t_icitem_view3_log' ,
                                    target_cloud_log_tableName ='rds_icitem_view3_log',
                                    field_selector=NA,
                                    pwd='') {
  table_createFromCopy_edgeToCloud(config_edge = config_edge,
                                   config_cloud = config_cloud ,
                                   src_edge_tableName = src_edge_log_tableName,
                                   target_cloud_tableName = target_cloud_log_tableName,
                                   field_selector = field_selector,pwd = pwd)
}




#8.2 云数据操作--------

#' 同步云端新增操作
#'
#' @param config 配置
#' @param data_tableName 数据表
#' @param FDiffAction  操作标识
#' @param input_tableName 输入表名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' cloud_sync_add()
cloud_sync_add <- function(config = config_cloud,
                           data_tableName='rds_icitem_view3',
                           input_tableName='rds_icitem_view3_input',
                           FDiffAction='add') {

  #连接
  conn <- conn_open(conn_config_info = config)
  field_list <- table_fieldList(config = config,table_name = input_tableName,exclude = 'FDiffAction')
  sql_add <-paste0("insert into ",data_tableName,"  select  ",field_list,"  from ",input_tableName," where FDiffAction = '",FDiffAction,"' ")
  try(tsda::sql_update(conn = conn,sql_str = sql_add))
  conn_close(conn)



}
#' 同步云数据删除部分
#'
#' @param config 配置
#' @param data_tableName 数据表
#' @param input_tableName 输入表
#' @param FInterID 内码
#' @param FDiffAction 操作码
#'
#' @return 返回值
#' @export
#'
#' @examples
#' cloud_sync_delete()
cloud_sync_delete <- function(config = config_cloud,
                              data_tableName='rds_icitem_view3',
                              input_tableName='rds_icitem_view3_input',
                              FInterID = 'FItemID',
                              FDiffAction='delete') {
  conn <- conn_open(conn_config_info = config)
  sql_del <- paste0("delete from ",data_tableName," where ",FInterID," in (select ",FInterID," from ",input_tableName," where fdiffaction = '",FDiffAction,"') ")
  try(tsda::sql_update(conn = conn,sql_str = sql_del))
  conn_close(conn)



}

#' 同步修改数据
#'
#' @param config 配置
#' @param data_tableName 数据表
#' @param input_tableName  缓存表
#' @param FInterID 内码
#'
#' @return 返回值
#' @export
#'
#' @examples
#' cloud_sync_modify()
cloud_sync_modify <- function(config = config_cloud,
                              data_tableName='rds_icitem_view3',
                              input_tableName='rds_icitem_view3_input',
                              FInterID = 'FItemID') {
  #先删除
  cloud_sync_delete(config = config,data_tableName = data_tableName,
                    input_tableName = input_tableName,FInterID = FInterID,FDiffAction = 'modify')

  #然后再新增即可
  cloud_sync_add(config = config,data_tableName = data_tableName,input_tableName = input_tableName,
                 FDiffAction = 'modify')


}

#' 云端清除缓存表
#'
#' @param config 配置表
#' @param input_tableName 输入表名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' cloud_clear_input()
cloud_clear_input <- function(config = config_cloud,input_tableName='rds_icitem_view3_input') {

  table_truncate(config = config,table_name = input_tableName,pwd = 'rds@2020')

}

#' 设置云端日志状态
#'
#' @param config 配置
#' @param log_tableName 日志表名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' cloud_log_setDone()
cloud_log_setDone <- function(config = config_cloud,log_tableName='rds_icitem_view3_log') {
  table_setBy_Status(config = config,table_name = log_tableName)

}





#9、automation--------


#' 定义主数据的元数据
#'
#' @param data_table  数据表
#' @param selector 字段
#' @param FInterID 关键字
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mdm_meta()
mdm_meta <- function(data_table='t_icitem',
                     selector=c('FItemID','FNumber','FName','FModel','FChartNumber','FErpClsID'),
                     FInterID='FItemID') {
  #针对数据进行预处理
  var_edge_data_view <- paste0(data_table,"_view")
  var_edge_image_name <- paste0(data_table,'_img')
  var_edge_diff_add_view_name <- paste0(data_table,'_diff_add')
  var_edge_diff_del_view_name <- paste0(data_table,'_diff_del')
  var_edge_diff_modify_view_name <- paste0(data_table,'_diff_modify')
  var_edge_diff_view_name <- paste0(data_table,'_diff')
  var_edge_log_table_name <- paste0(data_table,'_log')
  var_edge_log_func_maxVer <- paste0('func_',data_table,'_maxVer')
  var_cloud_data_table <- paste0('rds_',data_table)
  var_cloud_data_input <-paste0('rds_',data_table,'_input')
  var_cloud_log_table_name <-paste0('rds_',data_table,'_log')


  #格式化
  res <- list(edge_data_table=data_table,
              edge_view_selector=selector,
              edge_data_view=var_edge_data_view,
              edge_image_name=var_edge_image_name,
              edge_key_col=FInterID,
              edge_diff_add_view_name=var_edge_diff_add_view_name,
              edge_diff_del_view_name=var_edge_diff_del_view_name,
              edge_diff_modify_view_name=var_edge_diff_modify_view_name,
              edge_diff_view_name=var_edge_diff_view_name,
              edge_log_table_name=var_edge_log_table_name,
              edge_log_func_maxVer=var_edge_log_func_maxVer,
              cloud_data_table=var_cloud_data_table,
              cloud_data_input=var_cloud_data_input,
              cloud_log_table_name=var_cloud_log_table_name

  )
  return(res)

}


#' 主数据创建服务
#'
#' @param config_edge 边配置
#' @param config_cloud 云配置
#' @param config_meta 元数据
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mdm_init()
mdm_init <- function(config_edge=cfg_edge,
                             config_cloud=cfg_cloud,
                             config_meta=mdm_meta()) {
  # print(config_meta)

  #创建业务视图
  edge_data_view(config = config_edge,
                 table_name = config_meta$edge_data_table,
                 view_name = config_meta$edge_data_view,
                 selector = config_meta$edge_view_selector)
  #创建业务镜像
  edge_data_img(config = config_edge,
                src_view_name =config_meta$edge_data_view,
                target_table_name = config_meta$edge_image_name,
                pwd = 'rds@2020' )
  #创建差异视图
  # 新增部分
  edge_diff_add(config = config_edge,
                src_view_name = config_meta$edge_data_view,
                img_table_name = config_meta$edge_image_name,
                target_view_name = config_meta$edge_diff_add_view_name,
                key_col = config_meta$edge_key_col
  )
  #删除部分
  edge_diff_del(config = config_edge,
                src_view_name =config_meta$edge_data_view,
                img_table_name = config_meta$edge_image_name,
                target_view_name = config_meta$edge_diff_del_view_name,
                key_col = config_meta$edge_key_col)
  #修改部分
  edge_diff_modify(config = config_edge,
                   src_view_name =config_meta$edge_data_view,
                   img_table_name = config_meta$edge_image_name,
                   target_view_name =config_meta$edge_diff_modify_view_name,
                   key_col =config_meta$edge_key_col   )
  #全部的修改记录
  edge_diff_all(config = config_edge,
                diff_add_view_name = config_meta$edge_diff_add_view_name,
                diff_del_view_name =config_meta$edge_diff_del_view_name ,
                diff_modify_view_name =config_meta$edge_diff_modify_view_name,
                target_view_name = config_meta$edge_diff_view_name )
  #创建日志
  edge_Log_create(config = config_edge,
                  src_table_name =config_meta$edge_diff_view_name,
                  target_table_name =config_meta$edge_log_table_name,
                  pwd = 'rds@2020'
  )
  #创建最大版本号辅助函数
  edge_log_maxVersion(config = config_edge,log_table_name =config_meta$edge_log_table_name,
                      func_name = config_meta$edge_log_func_maxVer)
  #创建云端表结构
  cloud_createTable_data(config_edge = config_edge,
                         config_cloud = config_cloud,
                         src_edge_viewName = config_meta$edge_data_view,
                         target_cloud_tableName =config_meta$cloud_data_table,pwd = 'rds@2020')
  #创建云端缓存文件
  cloud_createTable_input(config_edge = config_edge,
                          config_cloud = config_cloud,
                          src_edge_diff_tableName = config_meta$edge_diff_view_name,
                          target_cloud_input_tableName =config_meta$cloud_data_input,
                          pwd = 'rds@2020'
  )
  #创建云端日志
  cloud_createTable_log(config_edge = config_edge,config_cloud = config_cloud,
                        src_edge_log_tableName =config_meta$edge_log_table_name,
                        target_cloud_log_tableName = config_meta$cloud_log_table_name,
                        pwd = 'rds@2020'  )







}


#' 主数据同步
#'
#' @param config_edge 边配置
#' @param config_cloud 云配置
#' @param config_meta 元数据
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mdm_autoRun()
mdm_autoRun <- function(config_edge=cfg_edge,
                                config_cloud=cfg_cloud,
                                config_meta=mdm_meta()) {
  #step1 推送日志
  edge_diff_pushToLog(config = config_edge,
                      log_table_name = config_meta$edge_log_table_name,
                      func_name = config_meta$edge_log_func_maxVer,
                      edge_diff_view_name =config_meta$edge_diff_view_name )
  #step2 推送数据，推送日志
  #数据文件
  edge_log_pushToCloud_input(config_edge = config_edge,config_cloud = config_cloud,
                             input_table_name = config_meta$cloud_data_input,
                             edge_diff_view_name = config_meta$edge_diff_view_name)
  #日志文件
  edge_log_pushToCloud_log(config_edge = config_edge,config_cloud = config_cloud,
                           cloud_log_table_name = config_meta$cloud_log_table_name,
                           edge_log_table_name = config_meta$edge_log_table_name)
  #step3 云端数据操作
  cloud_sync_add(config = config_cloud,data_tableName = config_meta$cloud_data_table,
                 input_tableName = config_meta$cloud_data_input)
  cloud_sync_delete(config = config_cloud,
                    data_tableName = config_meta$cloud_data_table,
                    input_tableName = config_meta$cloud_data_input,FInterID = config_meta$edge_key_col )
  cloud_sync_modify(config = config_cloud,
                    data_tableName = config_meta$cloud_data_table,
                    input_tableName =config_meta$cloud_data_input,
                    FInterID = config_meta$edge_key_col )
  #删除缓存文件
  cloud_clear_input(config = config_cloud,input_tableName = config_meta$cloud_data_input)

  #step4 设置云端日志
  cloud_log_setDone(config = config_cloud,log_tableName = config_meta$cloud_log_table_name)
  #step5推送镜像文件
  edge_view_pushToImg(config = config_edge,
                      src_view_name = config_meta$edge_data_view,
                      img_table_name = config_meta$edge_image_name,pwd = 'rds@2020')

}


