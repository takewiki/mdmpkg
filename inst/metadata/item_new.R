#查看上级物料是否存在-----

USE [AIS20140904110155]
GO

drop table t_Item_rds

/****** Object:  Table [dbo].[t_Item]    Script Date: 2021/1/4 17:02:17 ******/
  SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[t_Item_rds](
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
) ON [PRIMARY]
GO
