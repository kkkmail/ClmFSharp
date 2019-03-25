IF OBJECT_ID('[dbo].[ClmTask]') IS NULL begin
	print 'Creating table [dbo].[ClmTask] ...'

	CREATE TABLE [dbo].[ClmTask](
		[clmTaskId] [bigint] IDENTITY(1,1) NOT NULL,
		[clmDefaultValueId] [bigint] NOT NULL,
		[numberOfAminoAcids] [int] NOT NULL,
		[maxPeptideLength] [int] NOT NULL,
		[numberOfRepetitions] [int] NULL DEFAULT ((1)),
		[remainingRepetitions] [int] NULL DEFAULT ((1)),
		[createdOn] [datetime] NOT NULL DEFAULT (getdate()),
	 CONSTRAINT [PK_ClmTask] PRIMARY KEY CLUSTERED 
	(
		[clmTaskId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]
end else begin
	print 'Table [dbo].[ClmTask] already exists ...'
end
go



