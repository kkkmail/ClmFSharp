IF OBJECT_ID('[dbo].[WorkerNode]') IS NULL begin
	print 'Creating table [dbo].[WorkerNode] ...'

	CREATE TABLE [dbo].[WorkerNode](
		[workerNodeId] [uniqueidentifier] NOT NULL,
		[workerNodeName] [nvarchar](100) NOT NULL,
		[description] [nvarchar](1000) NULL,
		[createdOn] [datetime] NOT NULL,
	 CONSTRAINT [PK_WorkerNode] PRIMARY KEY CLUSTERED 
	(
		[workerNodeId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]

	ALTER TABLE [dbo].[WorkerNode] ADD  CONSTRAINT [DF_WorkerNode_createdOn]  DEFAULT (getdate()) FOR [createdOn]

	CREATE UNIQUE NONCLUSTERED INDEX [IX_WorkerNode] ON [dbo].[WorkerNode]
	(
		[workerNodeName] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
end else begin
	print 'Table [dbo].[WorkerNode] already exists ...'
end
go

