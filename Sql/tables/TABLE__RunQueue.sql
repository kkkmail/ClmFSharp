IF OBJECT_ID('[dbo].[RunQueue]') IS NULL begin
	print 'Creating table [dbo].[RunQueue] ...'

	CREATE TABLE [dbo].[RunQueue](
		[runQueueId] [uniqueidentifier] NOT NULL,
		[commandLineParamId] [uniqueidentifier] NOT NULL,
		[statusId] [int] NOT NULL DEFAULT ((0)),
		[createdOn] [datetime] NOT NULL DEFAULT (getdate()),
	 CONSTRAINT [PK_RunQueue] PRIMARY KEY CLUSTERED 
	(
		[runQueueId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]

	ALTER TABLE [dbo].[RunQueue] WITH CHECK ADD CONSTRAINT [FK_RunQueue_CommandLineParam] FOREIGN KEY([commandLineParamId])
	REFERENCES [dbo].[CommandLineParam] ([commandLineParamId])

	ALTER TABLE [dbo].[RunQueue] CHECK CONSTRAINT [FK_RunQueue_CommandLineParam]
end else begin
	print 'Table [dbo].[RunQueue] already exists ...'
end
go



