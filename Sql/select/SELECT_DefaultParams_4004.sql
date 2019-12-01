use clm4004
go

select 
	clmDefaultValueId,
	isnull(cast(dbo.getWasteRecyclingRate(clmDefaultValueId) as nvarchar(20)), '') as wasteRecyclingRate,
	isnull(cast(dbo.getCatSynthSim(clmDefaultValueId) as nvarchar(20)), '') as catSynthSim,
	isnull(cast(dbo.getCatSynthScarcity(clmDefaultValueId) as nvarchar(20)), '') as catSynthScarcity,
	isnull(cast(dbo.getCatDestrSim(clmDefaultValueId) as nvarchar(20)), '') as catDestrSim,
	isnull(cast(dbo.getCatDestrScarcity(clmDefaultValueId) as nvarchar(20)), '') as catDestrScarcity,
	dbo.getGroupId(clmDefaultValueId) as groupId,
	description
from ClmDefaultValue
order by clmDefaultValueId

