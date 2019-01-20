CREATE TABLE [dbo].[ResultData](
	[resultDataId] [bigint] IDENTITY(1,1) NOT NULL,
	[modelDataId] [bigint] NOT NULL,
	[numberOfAminoAcids] [int] NOT NULL,
	[maxPeptideLength] [int] NOT NULL,
	[y0] [money] NOT NULL,
	[tEnd] [money] NOT NULL,
	[useAbundant] [bit] NOT NULL,
	[maxEe] [float] NOT NULL,
	[maxAverageEe] [float] NOT NULL,
	[createdOn] [datetime] NOT NULL,
	[aminoAcids] [varbinary](max) NOT NULL,
	[allSubst] [varbinary](max) NOT NULL,
	[allInd] [varbinary](max) NOT NULL,
	[allRawReactions] [varbinary](max) NOT NULL,
	[allReactions] [varbinary](max) NOT NULL,
	[x] [varbinary](max) NOT NULL,
	[t] [varbinary](max) NOT NULL,
 CONSTRAINT [PK_ResultData] PRIMARY KEY CLUSTERED 
(
	[resultDataId] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

ALTER TABLE [dbo].[ResultData] ADD  CONSTRAINT [DF_ResultData_useAbundant]  DEFAULT ((0)) FOR [useAbundant]
GO

ALTER TABLE [dbo].[ResultData] ADD  CONSTRAINT [DF_ResultData_maxEe]  DEFAULT ((0)) FOR [maxEe]
GO

ALTER TABLE [dbo].[ResultData] ADD  CONSTRAINT [DF_ResultData_averageEe]  DEFAULT ((0)) FOR [maxAverageEe]
GO

ALTER TABLE [dbo].[ResultData] ADD  CONSTRAINT [DF_ResultData_createdOn]  DEFAULT (getdate()) FOR [createdOn]
GO

ALTER TABLE [dbo].[ResultData]  WITH CHECK ADD  CONSTRAINT [FK_ResultData_ResultData] FOREIGN KEY([modelDataId])
REFERENCES [dbo].[ModelData] ([modelDataId])
GO

ALTER TABLE [dbo].[ResultData] CHECK CONSTRAINT [FK_ResultData_ResultData]
GO


