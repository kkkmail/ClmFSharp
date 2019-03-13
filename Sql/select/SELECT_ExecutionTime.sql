--USE [Clm]
--GO

--SELECT [modelDataId]
--      ,[numberOfAminoAcids]
--      ,[maxPeptideLength]
--      ,[defaultSetIndex]
--      ,[fileStructureVersion]
--      ,[seedValue]
--      ,[modelDataParams]
--      ,[modelBinaryData]
--      ,[createdOn]
--  FROM [dbo].[ModelData]
--GO
--USE [Clm]
--GO

--SELECT [resultDataId]
--      ,[modelDataId]
--      ,[y0]
--      ,[tEnd]
--      ,[useAbundant]
--      ,[maxEe]
--      ,[maxAverageEe]
--      ,[createdOn]
--  FROM [dbo].[ResultData]
--GO


select
    --r.[modelDataId]
	m.fileStructureVersion
	--,[defaultSetIndex]
	--,[resultDataId]
    ,[y0]
    --,[tEnd]
    --,[useAbundant]
    --,[maxEe]
    --,[maxAverageEe]
 --   ,r.[createdOn] as completedOn
	--,m.[createdOn]
	, avg(cast(datediff(minute, m.[createdOn], r.[createdOn]) as money) / cast(60 as money)) as hourDiff
FROM [dbo].[ResultData] r
	inner join [dbo].[ModelData] m on m.modelDataId = r.modelDataId

where [defaultSetIndex] = 0 --and y0 = 20
group by m.fileStructureVersion, y0
order by fileStructureVersion, y0

