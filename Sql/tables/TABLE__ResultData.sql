IF OBJECT_ID('[dbo].[ResultData]') IS NULL begin
	print 'Creating table [dbo].[ResultData] ...'

	-- This is an "extension" table to CommandLineParam: resultDataId IS commandLineParamId.
	CREATE TABLE [dbo].[ResultData](
		[resultDataId] [uniqueidentifier] NOT NULL,
		[maxEe] [float] NOT NULL DEFAULT ((0)),
		[maxAverageEe] [float] NOT NULL DEFAULT ((0)),
		[maxWeightedAverageAbsEe] [float] NOT NULL DEFAULT ((0)),
		[maxLastEe] [float] NOT NULL DEFAULT ((0)),
		[createdOn] [datetime] NOT NULL DEFAULT (getdate()),
	 CONSTRAINT [PK_ResultData] PRIMARY KEY CLUSTERED ([resultDataId] ASC)
	)

	ALTER TABLE [dbo].[ResultData]  WITH CHECK ADD CONSTRAINT [FK_ResultData_CommandLineParam] FOREIGN KEY([resultDataId])
	REFERENCES [dbo].[CommandLineParam] ([commandLineParamId])

	ALTER TABLE [dbo].[ResultData] CHECK CONSTRAINT [FK_ResultData_CommandLineParam]
end else begin
	print 'Table [dbo].[ResultData] already exists ...'
end
go




