USE [Clm]
GO

SELECT [resultDataId]
      ,[modelDataId]
      ,[numberOfAminoAcids]
      ,[maxPeptideLength]

      ,[y0]
      ,[tEnd]
      ,[useAbundant]

      ,[maxEe]
      ,[maxAverageEe]
      ,[createdOn]

      --,[aminoAcids]
      --,[allSubst]
      --,[allInd]
      --,[allRawReactions]
      --,[allReactions]
      --,[x]
      --,[t]
FROM [dbo].[ResultData]
where 
	--[modelDataId] >= 26 and
	modelDataId in (select modelDataId from ModelData where defaultSetIndex = 4)
	and numberOfAminoAcids = 11
	and maxEe > 0.0001



select distinct modelDataId
from ResultData
where 
	--modelDataId >= 26 and
	modelDataId in (select modelDataId from ModelData where defaultSetIndex = 4)
	and numberOfAminoAcids = 11
	and maxEe > 0.0001



