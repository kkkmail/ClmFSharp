-- TODO kk:20200212 - Reference the source of the original code.
declare @table nvarchar(128)
declare @idcol nvarchar(128)
declare @sql nvarchar(max)

-- Initialize those two values.
set @table = 'ModelData'
set @idcol = 'modelDataId'

set @sql = 'select ' + @idcol +' , (0'

select @sql = @sql + ' + cast(isnull(datalength(' + name + '), 1) / 1048576.0 as money)' 
        from  sys.columns 
        where object_id = object_id(@table)
        and   is_computed = 0
set @sql = @sql + ') as rowSizeMB from ' + @table + ' order by rowSizeMB desc'

PRINT @sql

exec (@sql)

