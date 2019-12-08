IF OBJECT_ID('[dbo].[ResultData]') IS NULL begin
	print 'Creating table [dbo].[ResultData] ...'

	CREATE TABLE [dbo].[ResultData](
		[resultDataId] [uniqueidentifier] NOT NULL,
		[workerNodeId] [uniqueidentifier] NULL,
		[modelDataId] [uniqueidentifier] NOT NULL,
		[y0] [money] NOT NULL,
		[tEnd] [money] NOT NULL,
		[useAbundant] [bit] NOT NULL DEFAULT ((0)),
		[maxEe] [float] NOT NULL DEFAULT ((0)),
		[maxAverageEe] [float] NOT NULL DEFAULT ((0)),
		[maxWeightedAverageAbsEe] [float] NOT NULL DEFAULT ((0)),
		[maxLastEe] [float] NOT NULL DEFAULT ((0)),
		[createdOn] [datetime] NOT NULL DEFAULT (getdate()),
	 CONSTRAINT [PK_ResultData] PRIMARY KEY CLUSTERED 
	(
		[resultDataId] ASC
	)
	)

	ALTER TABLE [dbo].[ResultData]  WITH CHECK ADD  CONSTRAINT [FK_ResultData_ResultData] FOREIGN KEY([modelDataId])
	REFERENCES [dbo].[ModelData] ([modelDataId])

	ALTER TABLE [dbo].[ResultData] CHECK CONSTRAINT [FK_ResultData_ResultData]
end else begin
	print 'Table [dbo].[ResultData] already exists ...'
end
go




