USE [Clm]
GO

SELECT [modelDataId]
      ,[numberOfAminoAcids]
      ,[maxPeptideLength]
      ,[seedValue]
	  ,defaultSetIndex
      ,[fileStructureVersion]
      --,[modelData]
      ,[createdOn]
FROM [dbo].[ModelData]
--where [modelDataId] >= 26


