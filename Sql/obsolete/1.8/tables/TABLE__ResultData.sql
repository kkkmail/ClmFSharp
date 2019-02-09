IF OBJECT_ID('[dbo].[ResultData]') IS NULL begin
	print 'Creating table [dbo].[ResultData] ...'

	CREATE TABLE [dbo].[ResultData](
		[resultDataId] [bigint] IDENTITY(1,1) NOT NULL,
		[modelDataId] [bigint] NOT NULL,
		[numberOfAminoAcids] [int] NOT NULL,
		[maxPeptideLength] [int] NOT NULL,
		[y0] [money] NOT NULL,
		[tEnd] [money] NOT NULL,
		[useAbundant] [bit] NOT NULL DEFAULT ((0)),
		[maxEe] [float] NOT NULL DEFAULT ((0)),
		[maxAverageEe] [float] NOT NULL DEFAULT ((0)),
		[createdOn] [datetime] NOT NULL DEFAULT (getdate()),
		[aminoAcids] [varbinary](max) NULL,
		[allSubst] [varbinary](max) NULL,
		[allInd] [varbinary](max) NULL,
		[allRawReactions] [varbinary](max) NULL,
		[allReactions] [varbinary](max) NULL,
		[x] [varbinary](max) NULL,
		[t] [varbinary](max) NULL,
	 CONSTRAINT [PK_ResultData] PRIMARY KEY CLUSTERED 
	(
		[resultDataId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

	ALTER TABLE [dbo].[ResultData]  WITH CHECK ADD  CONSTRAINT [FK_ResultData_ResultData] FOREIGN KEY([modelDataId])
	REFERENCES [dbo].[ModelData] ([modelDataId])

	ALTER TABLE [dbo].[ResultData] CHECK CONSTRAINT [FK_ResultData_ResultData]
end else begin
	print 'Table [dbo].[ResultData] already exists ...'
end
go




