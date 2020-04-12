select 
	r.runQueueId,
	t.clmDefaultValueId,
	t.statusId,
	cast(datediff(second, r.createdOn, getdate()) / (24.0 * 3600) as decimal (10, 2)) as runTime
from
	RunQueue r
	inner join ModelData m on r.modelDataId = m.modelDataId
	inner join ClmTask t on m.clmTaskId = t.clmTaskId
order by statusId, 4 desc
