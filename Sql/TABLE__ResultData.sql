CREATE TABLE [dbo].[ResultData](
	[resultDataId] [bigint] IDENTITY(1,1) NOT NULL,
	[modelDataId] [bigint] NOT NULL,
	[y0] [money] NOT NULL,
	[tEnd] [money] NOT NULL,
	[allSubst] [nvarchar](max) NOT NULL,
	[allInd] [nvarchar](max) NOT NULL,
	[allRawReactions] [nvarchar](max) NOT NULL,
	[allReactions] [nvarchar](max) NOT NULL,
	[resultData] [nvarchar](max) NOT NULL,
	[maxEe] [float] NOT NULL,
 CONSTRAINT [PK_ResultData] PRIMARY KEY CLUSTERED 
(
	[resultDataId] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

ALTER TABLE [dbo].[ResultData]  WITH CHECK ADD  CONSTRAINT [FK_ResultData_ResultData] FOREIGN KEY([modelDataId])
REFERENCES [dbo].[ModelData] ([modelDataId])
GO

ALTER TABLE [dbo].[ResultData] CHECK CONSTRAINT [FK_ResultData_ResultData]
GO


