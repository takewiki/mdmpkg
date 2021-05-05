-- !preview conn=DBI::dbConnect(RSQLite::SQLite())
---查看RDS物料是否存在
Select * From (Select t1.* From t_Item t1  WHERE t1.FDeleteD=0  And t1.FItemClassID = 4 And t1.FItemClassID = 4)
i  Where 1=1  And
(FNumber = 'RDS')
go
