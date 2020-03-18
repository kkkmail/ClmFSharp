declare @now datetime
set @now = getdate()

;with w as
(
select [runQueueId]
      ,[runQueueOrder]
      ,[modelDataId]
      ,[runQueueStatusId]
      ,[progress]
      ,[y0]
      ,[tEnd]
      ,[useAbundant]
      ,[workerNodeId]
      ,[createdOn]
      ,[startedOn]
      ,[modifiedOn]
	  ,case when startedOn is not null and progress > 0 then dateadd(second, datediff(second, startedOn, @now) / progress, startedOn) else null end as estCompl
from [clm5001].[dbo].[RunQueue]
--where [progress] = -1000
where [progress] <> 1 and [runQueueStatusId] = 2
)
select * from w
--order by [runQueueOrder]
--order by [progress] desc
where estCompl is not null and estCompl < dateadd(day, 1, @now)
order by estCompl

