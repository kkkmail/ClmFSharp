use clm502
go

select count(*) as noOfResults from ResultData
go


-- Calculates how often the symmetry is broken for all processed default sets.
declare @maxWeightedAverageAbsEe float, @maxLastEe float, @runeTimeEst float
set @maxWeightedAverageAbsEe = 0.003
set @maxLastEe = 0.003
set @runeTimeEst = 1.60

; with
a as
(
	select
		d.clmDefaultValueId as defaultSetIndex,
		t.numberOfAminoAcids,
		--sum(case when t.clmTaskStatusId = 0 then t.remainingRepetitions else 0 end) as remainingRepetitions
		sum(
			case
				when q.RunQueueStatusId = 0 then 1.0 
				when q.RunQueueStatusId = 2 then (1.0 - q.progress) 
				else 0 
		end) as remainingRepetitions
	from
		ClmDefaultValue d 
		inner join ClmTask t on d.clmDefaultValueId = t.clmDefaultValueId
		inner join ModelData m on m.clmTaskId = t.clmTaskId
		inner join RunQueue q on q.modelDataId = m.modelDataId
	group by d.clmDefaultValueId, t.numberOfAminoAcids
),
b as
(
	select
		d.clmDefaultValueId as defaultSetIndex,
		t.numberOfAminoAcids,
		r.modelDataId,
		case 
			when r.maxWeightedAverageAbsEe > @maxWeightedAverageAbsEe or r.maxLastEe > @maxLastEe then 1 
			else 0 
		end as isSymmetryBroken,
		cast(datediff(minute, isnull(q.startedOn, m.createdOn), r.createdOn) as float) / 1440.0 as runTime
	from
		ClmDefaultValue d 
		inner join ClmTask t on d.clmDefaultValueId = t.clmDefaultValueId
		inner join ModelData m on t.clmTaskId = m.clmTaskId
		inner join ResultData r on m.modelDataId = r.modelDataId
		inner join RunQueue q on r.resultDataId = q.runQueueId
	where r.maxEe <= 1
),
c as
(
	select distinct
		defaultSetIndex,
		numberOfAminoAcids,
		modelDataId,
		max(isSymmetryBroken) as isSymmetryBroken,
		avg(runTime) as runTime
	from b
	group by defaultSetIndex, numberOfAminoAcids, modelDataId
),
d as
(
	select
		defaultSetIndex,
		numberOfAminoAcids,
		count(*) as modelCount,
		avg(runTime) as runTime
	from c
	group by defaultSetIndex, numberOfAminoAcids
),
e as
(
	select
		defaultSetIndex,
		numberOfAminoAcids,
		count(*) as symmBrokenCount
	from c
	where isSymmetryBroken = 1
	group by defaultSetIndex, numberOfAminoAcids
),
f as
(
	select
		a.numberOfAminoAcids,
		a.defaultSetIndex,
		isnull(d.modelCount, 0) as modelCount,
		isnull(e.symmBrokenCount, 0) as symmBrokenCount,
		cast(isnull(cast(isnull(e.symmBrokenCount, 0) as float) / cast(d.modelCount as float), 0) as money) as symmBrokenPct,
		isnull(cast(cast(d.runTime as decimal(10, 2)) as nvarchar(20)), '') as runTime,
		isnull(cast(dbo.getWasteRecyclingRate(a.defaultSetIndex) as nvarchar(20)), '') as wasteRecyclingRate,
		isnull(cast(dbo.getCatSynthSim(a.defaultSetIndex) as nvarchar(20)), '') as catSynthSim,
		isnull(cast(dbo.getCatSynthScarcity(a.defaultSetIndex) as nvarchar(20)), '') as catSynthScarcity,
		isnull(cast(dbo.getCatDestrSim(a.defaultSetIndex) as nvarchar(20)), '') as catDestrSim,
		isnull(cast(dbo.getCatDestrScarcity(a.defaultSetIndex) as nvarchar(20)), '') as catDestrScarcity,
		isnull(cast(dbo.getCatLigScarcity(a.defaultSetIndex) as nvarchar(20)), '') as catLigScarcity,
		isnull(cast(dbo.getCatLigMult(a.defaultSetIndex) as nvarchar(20)), '') as catLigMult,
		cast(a.remainingRepetitions * isnull(d.runTime, @runeTimeEst) as decimal(10, 2)) as remainingRunTime
	from a
		left outer join d on a.defaultSetIndex = d.defaultSetIndex and a.numberOfAminoAcids = d.numberOfAminoAcids
		left outer join e on a.defaultSetIndex = e.defaultSetIndex and a.numberOfAminoAcids = e.numberOfAminoAcids
)

select 
	* 
from f
order by defaultSetIndex
