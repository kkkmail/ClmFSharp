CREATE TABLE [dbo].[ModelData](
	[modelDataId] [bigint] IDENTITY(1,1) NOT NULL,
	[numberOfAminoAcids] [int] NOT NULL,
	[maxPeptideLength] [int] NOT NULL,
	[seedValue] [int] NULL,
	[defaultSetIndex] [int] NOT NULL,
	[fileStructureVersion] [nvarchar](50) NOT NULL,
	[modelData] [varbinary](max) NULL,
	[createdOn] [datetime] NOT NULL,
 CONSTRAINT [PK_ModelData] PRIMARY KEY CLUSTERED 
(
	[modelDataId] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

ALTER TABLE [dbo].[ModelData] ADD  CONSTRAINT [DF_ModelData_defaultSetId]  DEFAULT ((-1)) FOR [defaultSetIndex]
GO

ALTER TABLE [dbo].[ModelData] ADD  CONSTRAINT [DF_ModelData_createdOn]  DEFAULT (getdate()) FOR [createdOn]
GO


