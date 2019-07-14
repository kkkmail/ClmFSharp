USE [Clm]
GO

SELECT [resultDataId]
      ,r.[modelDataId]
	  ,m.defaultSetIndex
      ,[y0]
      ,[tEnd]
      ,[useAbundant]
      ,[maxEe]
      ,[maxAverageEe]
	  ,m.createdOn as modelCreatedOn
      ,r.[createdOn]
  FROM [dbo].[ResultData] r inner join ModelData m on r.modelDataId = m.modelDataId
  --where modelDataId >= 35
  order by modelDataId, y0
GO


