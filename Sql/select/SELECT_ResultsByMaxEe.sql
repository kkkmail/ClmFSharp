-- Calculates how often the symmetry is broken for all processed default sets.
declare @maxEeThreshold float, @numberOfAminoAcids int
set @numberOfAminoAcids = 20
set @maxEeThreshold = 0.00001


; with
w as
	(
	select 
		r.modelDataId, 
		(select top 1 defaultSetIndex from ModelData where modelDataId = r.modelDataId) as defaultSetIndex,
		case when r.maxEe > @maxEeThreshold then 1 else 0 end as isSymmetryBroken
	from ResultData r inner join ModelData m on r.modelDataId = m.modelDataId
	where m.numberOfAminoAcids = @numberOfAminoAcids
),
u as
(
	select distinct 
		modelDataId,
		defaultSetIndex,
		max(isSymmetryBroken) as isSymmetryBroken
	from w
	group by modelDataId, defaultSetIndex
),
d as
(
	select 
		defaultSetIndex,
		count(*) as modelCount
	from u
	group by defaultSetIndex
),
b as
(
	select 
		defaultSetIndex,
		count(*) as symmBrokenCount
	from u
	where isSymmetryBroken = 1
	group by defaultSetIndex
)
select 
	d.defaultSetIndex,
	d.modelCount,
	isnull(b.symmBrokenCount, 0) as symmBrokenCount,
	cast(isnull(b.symmBrokenCount, 0) as float) / cast(d.modelCount as float) as symmBrokenPct
from d left outer join b on d.defaultSetIndex = b.defaultSetIndex
order by d.defaultSetIndex
