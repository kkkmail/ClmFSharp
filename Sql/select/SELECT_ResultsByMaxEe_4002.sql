use clm4002
go

select count(*) as noOfResults from ResultData
go


-- Calculates how often the symmetry is broken for all processed default sets.
--declare @maxEe float, @maxAverageEe float
--set @maxEe = 0.0001
--set @maxAverageEe = 0.000003

declare @maxWeightedAverageAbsEe float, @maxLastEe float, @unknownRunTime float
set @unknownRunTime = 2.0
set @maxWeightedAverageAbsEe = 0.0001
set @maxLastEe = 0.0001

; with
w as
(
	select 
		m.modelDataId, 
		t.numberOfAminoAcids,
		case when statusId = 0 then t.remainingRepetitions else 0 end as remainingRepetitions,
		(select top 1 t.clmDefaultValueId from ClmTask t inner join ModelData on t.clmTaskId = m.clmTaskId where m.modelDataId = r.modelDataId) as defaultSetIndex,
		case 
			--when r.maxEe > @maxEe and r.maxAverageEe > @maxAverageEe then 1 
			when r.maxWeightedAverageAbsEe > @maxWeightedAverageAbsEe or r.maxLastEe > @maxLastEe then 1 
			else 0 
		end as isSymmetryBroken,
		cast(datediff(minute, m.createdOn, r.createdOn) as float) / 1440.0 as runTime
	from
		ClmTask t
		left outer join ModelData m on t.clmTaskId = m.clmTaskId 
		left outer join ResultData r on m.modelDataId = r.modelDataId
	 
),
u as
(
	select distinct 
		modelDataId,
		numberOfAminoAcids,
		defaultSetIndex,
		isnull(max(isSymmetryBroken), 0) as isSymmetryBroken,
		isnull(avg(runTime), @unknownRunTime) as runTime,
		remainingRepetitions
	from w
	group by modelDataId, numberOfAminoAcids, defaultSetIndex, remainingRepetitions
),
d as
(
	select 
		numberOfAminoAcids,
		defaultSetIndex,
		count(*) as modelCount,
		isnull(avg(runTime), @unknownRunTime) as runTime,
		remainingRepetitions
	from u
	group by numberOfAminoAcids, defaultSetIndex, remainingRepetitions
),
b as
(
	select 
		numberOfAminoAcids,
		defaultSetIndex,
		count(*) as symmBrokenCount,
		remainingRepetitions
	from u
	where isSymmetryBroken = 1
	group by numberOfAminoAcids, defaultSetIndex, remainingRepetitions
)
select 
	d.numberOfAminoAcids,
	d.defaultSetIndex,
	d.modelCount,
	isnull(b.symmBrokenCount, 0) as symmBrokenCount,
	cast(cast(isnull(b.symmBrokenCount, 0) as float) / cast(d.modelCount as float) as money) as symmBrokenPct,
	cast(d.runTime as decimal(10, 2)) as runTime,
	isnull(cast(dbo.getWasteRecyclingRate(d.defaultSetIndex) as nvarchar(20)), '') as wasteRecyclingRate,
	isnull(cast(dbo.getCatSynthSim(d.defaultSetIndex) as nvarchar(20)), '') as catSynthSim,
	isnull(cast(dbo.getCatSynthScarcity(d.defaultSetIndex) as nvarchar(20)), '') as catSynthScarcity,
	isnull(cast(dbo.getCatDestrSim(d.defaultSetIndex) as nvarchar(20)), '') as catDestrSim,
	isnull(cast(dbo.getCatDestrScarcity(d.defaultSetIndex) as nvarchar(20)), '') as catDestrScarcity,
	cast(d.remainingRepetitions * d.runTime as decimal(10, 2)) as remainingRunTime
from d left outer join b on d.defaultSetIndex = b.defaultSetIndex
order by d.numberOfAminoAcids, d.defaultSetIndex
