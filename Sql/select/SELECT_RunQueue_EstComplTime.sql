use clm5001
go

declare @now datetime
set @now = getdate()

;with w as
(
	select
		runQueueOrder
		,d.clmDefaultValueId
		,progress
		,case when startedOn is not null and progress > 0 then dateadd(second, datediff(second, startedOn, @now) / progress, startedOn) else null end as estCompl
		,cast(case when startedOn is not null and progress > 0 then datediff(second, startedOn, @now) / progress / (3600 * 24) else null end as money) as totalRunTime
		,runQueueId
		,workerNodeId
	from
		ClmDefaultValue d 
		inner join ClmTask t on d.clmDefaultValueId = t.clmDefaultValueId
		inner join ModelData m on m.clmTaskId = t.clmTaskId
		inner join RunQueue q on q.modelDataId = m.modelDataId
	where progress <> 1 and runQueueStatusId = 2
)
select * from w
where
	estCompl is not null
	and estCompl < dateadd(day, 1, @now)
order by estCompl
