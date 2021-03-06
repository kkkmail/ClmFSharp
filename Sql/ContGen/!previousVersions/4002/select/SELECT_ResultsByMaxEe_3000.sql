use clm3100
go

select count(*) as noOfResults from ResultData
go


-- Calculates how often the symmetry is broken for all processed default sets.
declare @maxEe float, @maxAverageEe float
set @maxEe = 0.00001
set @maxAverageEe = 0.000003

; with
w as
	(
	select 
		r.modelDataId, 
		t.numberOfAminoAcids,
		(select top 1 t.clmDefaultValueId from ClmTask t inner join ModelData on t.clmTaskId = m.clmTaskId where m.modelDataId = r.modelDataId) as defaultSetIndex,
		case when r.maxEe > @maxEe and r.maxAverageEe > @maxAverageEe then 1 else 0 end as isSymmetryBroken,
		cast(datediff(minute, m.createdOn, r.createdOn) as float) / 1440.0 as runTime
	from ResultData r inner join ModelData m on r.modelDataId = m.modelDataId inner join ClmTask t on m.clmTaskId = t.clmTaskId
),
u as
(
	select distinct 
		modelDataId,
		numberOfAminoAcids,
		defaultSetIndex,
		max(isSymmetryBroken) as isSymmetryBroken,
		avg(runTime) as runTime
	from w
	group by modelDataId, numberOfAminoAcids, defaultSetIndex
),
d as
(
	select 
		numberOfAminoAcids,
		defaultSetIndex,
		count(*) as modelCount,
		avg(runTime) as runTime
	from u
	group by numberOfAminoAcids, defaultSetIndex
),
b as
(
	select 
		numberOfAminoAcids,
		defaultSetIndex,
		count(*) as symmBrokenCount
	from u
	where isSymmetryBroken = 1
	group by numberOfAminoAcids, defaultSetIndex
)
select 
	d.numberOfAminoAcids,
	d.defaultSetIndex,
	d.modelCount,
	isnull(b.symmBrokenCount, 0) as symmBrokenCount,
	cast(cast(isnull(b.symmBrokenCount, 0) as float) / cast(d.modelCount as float) as money) as symmBrokenPct,
	cast(d.runTime as decimal(10, 2)) as runTime
from d left outer join b on d.defaultSetIndex = b.defaultSetIndex
order by d.numberOfAminoAcids, d.defaultSetIndex

