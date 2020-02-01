;with 
	valTbl as
	(
		select * 
		from 
		( values
			  (0, 'Active')
			, (1, 'Inactive')

		) as a (clmTaskStatusId, clmTaskStatusName)
	)
insert into clmTaskStatus
select valTbl.*
from valTbl
left outer join clmTaskStatus on valTbl.clmTaskStatusId = ClmTaskStatus.clmTaskStatusId
where ClmTaskStatus.clmTaskStatusId is null
go


