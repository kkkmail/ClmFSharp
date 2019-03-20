USE [Clm]
GO

SELECT [modelDataId]
      ,[numberOfAminoAcids]
      ,[maxPeptideLength]
      ,[defaultSetIndex]
      ,[fileStructureVersion]
      ,[seedValue]
      ,hashbytes('MD5',[modelDataParams]) as modelDataParamsHash
      ,hashbytes('MD5', [modelBinaryData]) as modelBinaryDataHash
      ,[createdOn]
  FROM [dbo].[ModelData]
GO


