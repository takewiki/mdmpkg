# 创建表结构--------
#' 创建基础资料主数据的副表，用于存储待分配的物料
#'
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' md_createTable_unAllocated
md_createTable_unAllocated <- function(conn=vmrdspkg::conn_vm_erp_test(),table_name='t_Item_rds') {
  sql <- paste0("CREATE TABLE [dbo].[",table_name,"](
	[FItemID] [int] NOT NULL,
	[FItemClassID] [int] NOT NULL,
	[FExternID] [int] NOT NULL,
	[FNumber] [varchar](80) NOT NULL,
	[FParentID] [int] NOT NULL,
	[FLevel] [smallint] NOT NULL,
	[FDetail] [bit] NOT NULL,
	[FName] [varchar](255) NOT NULL,
	[FUnUsed] [bit] NULL,
	[FBrNo] [varchar](10) NOT NULL,
	[FFullNumber] [varchar](80) NOT NULL,
	[FDiff] [bit] NOT NULL,
	[FDeleted] [smallint] NOT NULL,
	[FShortNumber] [varchar](80) NULL,
	[FFullName] [varchar](255) NULL,
	[UUID] [uniqueidentifier] NOT NULL,
	[FGRCommonID] [int] NOT NULL,
	[FSystemType] [int] NOT NULL,
	[FUseSign] [int] NOT NULL,
	[FChkUserID] [int] NULL,
	[FAccessory] [smallint] NOT NULL,
	[FGrControl] [int] NOT NULL,
	[FModifyTime] [timestamp] NOT NULL,
	[FHavePicture] [smallint] NOT NULL,
 CONSTRAINT [PK_Item_rds] PRIMARY KEY NONCLUSTERED
(
	[FItemID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY],
 CONSTRAINT [uk_Item2_rds] UNIQUE CLUSTERED
(
	[FItemClassID] ASC,
	[FNumber] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]")

	 tsda::sql_update(conn,sql)

}



#' 创建主数据备用表
#'
#' @param conn 连接
#' @param table_name 表名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' md_createTable_backup()
md_createTable_backup <- function(conn=vmrdspkg::conn_vm_erp_test(),table_name='t_Item_rdsBak') {
  sql <- paste0("CREATE TABLE [dbo].[",table_name,"](
	[FItemID] [int] NOT NULL,
	[FItemClassID] [int] NOT NULL,
	[FExternID] [int] NOT NULL,
	[FNumber] [varchar](80) NOT NULL,
	[FParentID] [int] NOT NULL,
	[FLevel] [smallint] NOT NULL,
	[FDetail] [bit] NOT NULL,
	[FName] [varchar](255) NOT NULL,
	[FUnUsed] [bit] NULL,
	[FBrNo] [varchar](10) NOT NULL,
	[FFullNumber] [varchar](80) NOT NULL,
	[FDiff] [bit] NOT NULL,
	[FDeleted] [smallint] NOT NULL,
	[FShortNumber] [varchar](80) NULL,
	[FFullName] [varchar](255) NULL,
	[UUID] [uniqueidentifier] NOT NULL,
	[FGRCommonID] [int] NOT NULL,
	[FSystemType] [int] NOT NULL,
	[FUseSign] [int] NOT NULL,
	[FChkUserID] [int] NULL,
	[FAccessory] [smallint] NOT NULL,
	[FGrControl] [int] NOT NULL,
	[FModifyTime] [timestamp] NOT NULL,
	[FHavePicture] [smallint] NOT NULL,
 CONSTRAINT [PK_Item_rdsBak] PRIMARY KEY NONCLUSTERED
(
	[FItemID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY],
 CONSTRAINT [uk_Item2_rdsBak] UNIQUE CLUSTERED
(
	[FItemClassID] ASC,
	[FNumber] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]")

	 tsda::sql_update(conn,sql)

}


#' 建议基础资料的分配规则表
#'
#' @param conn 连接
#' @param table_name 表名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' md_createTable_room()
md_createTable_room <- function(conn=vmrdspkg::conn_vm_erp_test(),table_name='t_item_rdsRoom') {
  sql <- paste0("	Create table ",table_name,"  (
		[FItemClassID] [int] NOT NULL,
			[FItemID] [int] NOT NULL,

	[FNumber] [varchar](80) NOT NULL,
	[FName] [varchar](255) NOT NULL,
	FPropType varchar(30) Not Null,
	[FNumber_New] [varchar](80)  default('')  NOT NULL,
	FFlag int default(0))")

	 tsda::sql_update(conn,sql)

}




#将未分配的物料写入待分配表
#' 将外购的物料推入待分配表
#'
#' @param table_name_rds 物料明细表
#' @param table_name_room 待分配统计表
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#'  md_pushItem2UnAllocated_wg()
 md_pushItem2UnAllocated_wg <-function(conn=vmrdspkg::conn_vm_erp_test(),
                                       table_name_rds='t_Item_rds',
                                       table_name_room ='t_item_rdsRoom'
                                       ){

  sql_item <- paste0("select  count(1)  as Fcount
		   from t_item
		where FNumber like 'RDS.01.%'
		and fname = 'WG'")
  data_item <- tsda::sql_select(conn,sql_item)
  ncount <- data_item$Fcount
  if (ncount >0){
    #存在待处理的记录
    sql_wg <-paste0("
 INSERT INTO [dbo].[",table_name_rds,"]
           ([FItemID]
           ,[FItemClassID]
           ,[FExternID]
           ,[FNumber]
           ,[FParentID]
           ,[FLevel]
           ,[FDetail]
           ,[FName]
           ,[FUnUsed]
           ,[FBrNo]
           ,[FFullNumber]
           ,[FDiff]
           ,[FDeleted]
           ,[FShortNumber]
           ,[FFullName]
           ,[UUID]
           ,[FGRCommonID]
           ,[FSystemType]
           ,[FUseSign]
           ,[FChkUserID]
           ,[FAccessory]
           ,[FGrControl]
           ,[FHavePicture])
 select [FItemID]
           ,[FItemClassID]
           ,[FExternID]
           ,[FNumber]
           ,[FParentID]
           ,[FLevel]
           ,[FDetail]
           ,[FName]
           ,[FUnUsed]
           ,[FBrNo]
           ,[FFullNumber]
           ,[FDiff]
           ,[FDeleted]
           ,[FShortNumber]
           ,[FFullName]
           ,[UUID]
           ,[FGRCommonID]
           ,[FSystemType]
           ,[FUseSign]
           ,[FChkUserID]
           ,[FAccessory]
           ,[FGrControl]
           ,[FHavePicture]
		   from t_item
		where FNumber like 'RDS.01.%'
		and fname = 'WG'")
    #深度对数据进行数据
    try(tsda::sql_update(conn,sql_wg))

    sql_room <- paste0("insert into ",table_name_room,"
select
          [FItemClassID],

          [FItemID]


           ,[FNumber]

           ,[FName],
		   '外购' as FPropType,
		   '' as FNumber_New,
		   0 as FFlag

		   from t_item
		where FNumber like 'RDS.01.%'
		and fname = 'WG'")

    try(tsda::sql_update(conn,sql_room))

    sql_del <- paste0("delete from t_item
		where FNumber like 'RDS.01.%'
		and fname = 'WG'")
    try(tsda::sql_update(conn,sql_del))

    res <- TRUE
  }else{
    res <- FALSE
  }
   return(res)
 }




#' 针对自制物料进行初始化隐藏处理
#'
#' @param conn ERP链接信息
#' @param table_name_rds 物料明细表
#' @param table_name_room 待分配统计表
#'
#' @return 返回值
#' @export
#'
#' @examples
#' md_pushItem2UnAllocated_zz()
md_pushItem2UnAllocated_zz <-function(conn=vmrdspkg::conn_vm_erp_test(),
                                       table_name_rds='t_Item_rds',
                                       table_name_room ='t_item_rdsRoom'
 ){

   sql_item <- paste0("select  count(1)  as Fcount
		   from t_item
		where FNumber like 'RDS.02.%'
		and fname = 'ZZ'")
   data_item <- tsda::sql_select(conn,sql_item)
   ncount <- data_item$Fcount
   if (ncount >0){
     #存在待处理的记录
     sql_wg <-paste0("
 INSERT INTO [dbo].[",table_name_rds,"]
           ([FItemID]
           ,[FItemClassID]
           ,[FExternID]
           ,[FNumber]
           ,[FParentID]
           ,[FLevel]
           ,[FDetail]
           ,[FName]
           ,[FUnUsed]
           ,[FBrNo]
           ,[FFullNumber]
           ,[FDiff]
           ,[FDeleted]
           ,[FShortNumber]
           ,[FFullName]
           ,[UUID]
           ,[FGRCommonID]
           ,[FSystemType]
           ,[FUseSign]
           ,[FChkUserID]
           ,[FAccessory]
           ,[FGrControl]
           ,[FHavePicture])
 select [FItemID]
           ,[FItemClassID]
           ,[FExternID]
           ,[FNumber]
           ,[FParentID]
           ,[FLevel]
           ,[FDetail]
           ,[FName]
           ,[FUnUsed]
           ,[FBrNo]
           ,[FFullNumber]
           ,[FDiff]
           ,[FDeleted]
           ,[FShortNumber]
           ,[FFullName]
           ,[UUID]
           ,[FGRCommonID]
           ,[FSystemType]
           ,[FUseSign]
           ,[FChkUserID]
           ,[FAccessory]
           ,[FGrControl]
           ,[FHavePicture]
		   from t_item
		where FNumber like 'RDS.02.%'
		and fname = 'ZZ'")
     #深度对数据进行数据
     try(tsda::sql_update(conn,sql_wg))

     sql_room <- paste0("insert into ",table_name_room,"
select
          [FItemClassID],

          [FItemID]


           ,[FNumber]

           ,[FName],
		   '自制' as FPropType,
		   '' as FNumber_New,
		   0 as FFlag

		   from t_item
		where FNumber like 'RDS.02.%'
		and fname = 'ZZ'")

     try(tsda::sql_update(conn,sql_room))

     sql_del <- paste0("delete from t_item
		where FNumber like 'RDS.02.%'
		and fname = 'ZZ'")
     try(tsda::sql_update(conn,sql_del))

     res <- TRUE
   }else{
     res <- FALSE
   }
   return(res)
 }



#' 处理委外待分配数据
#'
#' @param conn 连接
#' @param table_name_rds 物料明细表
#' @param table_name_room 待分配信息表
#'
#' @return 返回值
#' @export
#'
#' @examples
#' md_pushItem2UnAllocated_ww()
md_pushItem2UnAllocated_ww <-function(conn=vmrdspkg::conn_vm_erp_test(),
                                      table_name_rds='t_Item_rds',
                                      table_name_room ='t_item_rdsRoom'
){

  sql_item <- paste0("select  count(1)  as Fcount
		   from t_item
		where FNumber like 'RDS.03.%'
		and fname = 'WW'")
  data_item <- tsda::sql_select(conn,sql_item)
  ncount <- data_item$Fcount
  if (ncount >0){
    #存在待处理的记录
    sql_wg <-paste0("
 INSERT INTO [dbo].[",table_name_rds,"]
           ([FItemID]
           ,[FItemClassID]
           ,[FExternID]
           ,[FNumber]
           ,[FParentID]
           ,[FLevel]
           ,[FDetail]
           ,[FName]
           ,[FUnUsed]
           ,[FBrNo]
           ,[FFullNumber]
           ,[FDiff]
           ,[FDeleted]
           ,[FShortNumber]
           ,[FFullName]
           ,[UUID]
           ,[FGRCommonID]
           ,[FSystemType]
           ,[FUseSign]
           ,[FChkUserID]
           ,[FAccessory]
           ,[FGrControl]
           ,[FHavePicture])
 select [FItemID]
           ,[FItemClassID]
           ,[FExternID]
           ,[FNumber]
           ,[FParentID]
           ,[FLevel]
           ,[FDetail]
           ,[FName]
           ,[FUnUsed]
           ,[FBrNo]
           ,[FFullNumber]
           ,[FDiff]
           ,[FDeleted]
           ,[FShortNumber]
           ,[FFullName]
           ,[UUID]
           ,[FGRCommonID]
           ,[FSystemType]
           ,[FUseSign]
           ,[FChkUserID]
           ,[FAccessory]
           ,[FGrControl]
           ,[FHavePicture]
		   from t_item
		where FNumber like 'RDS.03.%'
		and fname = 'WW'")
    #深度对数据进行数据
    try(tsda::sql_update(conn,sql_wg))

    sql_room <- paste0("insert into ",table_name_room,"
select
          [FItemClassID],

          [FItemID]


           ,[FNumber]

           ,[FName],
		   '委外' as FPropType,
		   '' as FNumber_New,
		   0 as FFlag

		   from t_item
		where FNumber like 'RDS.03.%'
		and fname = 'WW'")

    try(tsda::sql_update(conn,sql_room))

    sql_del <- paste0("delete from t_item
		where FNumber like 'RDS.03.%'
		and fname = 'WW'")
    try(tsda::sql_update(conn,sql_del))

    res <- TRUE
  }else{
    res <- FALSE
  }
  return(res)
}



#' 返回未分配的物料数据
#'
#' @param conn 连接
#' @param n  返回数量
#' @param FPropType 物料属性
#'
#' @return 返回值
#' @export
#'
#' @examples
#' Item_getUnAllocateNumbers()
Item_getUnAllocateNumbers <- function(conn=vmrdspkg::conn_vm_erp_test(),
                               n=10,FPropType='外购') {
  #获取相应的数据物料数据，不考虑成本成本对象
  # 但是更新数据时需要考虑成本对象的
  sql <- paste0(" select  top  ",n,"  FNumber from t_item_rdsroom
 where  FPropType = '",FPropType,"' and FFlag = 0 and FItemClassId = 4
 order by FNumber")
  data <- tsda::sql_select(conn,sql)
  ncount <-nrow(data)
  if(ncount == n){
    res <- data
  }else{
    res <- NULL
  }
  return(res)

}



