
#物料元数据------

#' 定义物料同步的元数据
#'
#' @return 反回元数据列表
#' @export
#'
#' @examples
#' md_material_meta()
md_material_meta <- function() {
  res <- list(edge_data_table='t_icitem',
              edge_view_selector=c('FItemID','FNumber','FName','FModel','FChartNumber','FErpClsID'),
              edge_data_view='t_icitem_view',
              edge_image_name='t_icitem_img',
              edge_key_col='FItemID',
              edge_diff_add_view_name='t_icitem_diff_add',
              edge_diff_del_view_name='t_icitem_diff_del',
              edge_diff_modify_view_name='t_icitem_diff_modify',
              edge_diff_view_name='t_icitem_diff',
              edge_log_table_name='t_icitem_log',
              edge_log_func_maxVer='func_icitem_max_ver',
              cloud_data_table='rds_icitem',
              cloud_data_input='rds_icitem_input',
              cloud_log_table_name='rds_icitem_log'

              )
  return(res)

}

#物料初始化-----

#' 定义物料主数据初始化
#'
#' @param config_edge 边配置
#' @param config_cloud 云配置
#' @param config_meta 元灵气
#'
#' @return 返回值
#' @include util.R
#' @export
#'
#' @examples
#' md_material_init()
md_material_init <- function(config_edge=cfg_edge,
                             config_cloud=cfg_cloud,
                             config_meta=md_material_meta()) {
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
#物料同步运行

#' 物料同步
#'
#' @param config_edge 边配置
#' @param config_cloud 云配置
#' @param config_meta 物料元数据
#'
#' @return 返回值
#' @export
#'
#' @examples
#' md_material_autoRun()
md_material_autoRun <- function(config_edge=cfg_edge,
                                config_cloud=cfg_cloud,
                                config_meta=md_material_meta()) {
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
