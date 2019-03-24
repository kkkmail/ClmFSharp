IF OBJECT_ID('[dbo].[Task]') IS NULL begin
	print 'Creating table [dbo].[Task] ...'

	CREATE TABLE [dbo].[Task](
		[taskId] [int] IDENTITY(1,1) NOT NULL,
		[clmDefaultValueId] [bigint] NOT NULL,
		[numberOfAminoAcids] [int] NOT NULL,
		[maxPeptideLength] [int] NOT NULL,
		[y0] [money] NOT NULL,
		[tEnd] [money] NOT NULL,
		[useAbundant] [bit] NOT NULL DEFAULT ((0)),
		[repeat] [bit] NOT NULL DEFAULT ((0)),
		[completed] [bit] NOT NULL DEFAULT ((0)),
		[createdOn] [datetime] NOT NULL DEFAULT (getdate()),
	 CONSTRAINT [PK_Task] PRIMARY KEY CLUSTERED 
	(
		[taskId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]
end else begin
	print 'Table [dbo].[Task] already exists ...'
end
go



