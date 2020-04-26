IF OBJECT_ID('[dbo].[ClmDefaultValue]') IS NULL begin
	print 'Creating table [dbo].[ClmDefaultValue] ...'

	CREATE TABLE [dbo].[ClmDefaultValue](
		clmDefaultValueId [bigint] NOT NULL,
		[defaultRateParams] [nvarchar](max) NOT NULL,
		[description] nvarchar(2000) NULL,
		[fileStructureVersion] money NOT NULL,
	 CONSTRAINT [PK_ClmDefaultValue] PRIMARY KEY CLUSTERED 
	(
		clmDefaultValueId ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
end else begin
	print 'Table [dbo].[ClmDefaultValue] already exists ...'
end
go


IF OBJECT_ID('[dbo].[ClmTask]') IS NULL begin
	print 'Creating table [dbo].[ClmTask] ...'

	CREATE TABLE [dbo].[ClmTask](
		[clmTaskId] [bigint] IDENTITY(1,1) NOT NULL,
		[clmDefaultValueId] [bigint] NOT NULL,
		[numberOfAminoAcids] [int] NOT NULL,
		[maxPeptideLength] [int] NOT NULL,
		[numberOfRepetitions] [int] NOT NULL DEFAULT ((1)),
		[remainingRepetitions] [int] NOT NULL DEFAULT ((1)),
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



IF OBJECT_ID('[dbo].[CommandLineParam]') IS NULL begin
	print 'Creating table [dbo].[CommandLineParam] ...'

	CREATE TABLE [dbo].[CommandLineParam](
		commandLineParamId [bigint] IDENTITY(1,1) NOT NULL,
		[clmTaskId] [bigint] NOT NULL,
		[y0] [money] NOT NULL,
		[tEnd] [money] NOT NULL,
		[useAbundant] [bit] NOT NULL DEFAULT ((0)),
	 CONSTRAINT [PK_TCommandLineParam] PRIMARY KEY CLUSTERED 
	(
		commandLineParamId ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]

	ALTER TABLE [dbo].[CommandLineParam]  WITH CHECK ADD  CONSTRAINT [FK_CommandLineParam_ClmTask] FOREIGN KEY([clmTaskId])
	REFERENCES [dbo].ClmTask ([clmTaskId])

	ALTER TABLE [dbo].[CommandLineParam] CHECK CONSTRAINT [FK_CommandLineParam_ClmTask]
end else begin
	print 'Table [dbo].[CommandLineParam] already exists ...'
end
go



IF OBJECT_ID('[dbo].[ModelData]') IS NULL begin
	print 'Creating table [dbo].[ModelData] ...'

	CREATE TABLE [dbo].[ModelData](
		[modelDataId] [bigint] IDENTITY(1,1) NOT NULL,
		[clmTaskId] [bigint] NOT NULL,
		[fileStructureVersion] money NOT NULL,
		[seedValue] [int] NULL,
		[modelDataParams] [nvarchar](max) NOT NULL,
		[modelBinaryData] [varbinary](max) NOT NULL,
		[createdOn] [datetime] NOT NULL DEFAULT (getdate()),
	 CONSTRAINT [PK_ModelData] PRIMARY KEY CLUSTERED 
	(
		[modelDataId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

	ALTER TABLE [dbo].[ModelData]  WITH CHECK ADD  CONSTRAINT [FK_ModelData_ClmTask] FOREIGN KEY([clmTaskId])
	REFERENCES [dbo].ClmTask ([clmTaskId])

	ALTER TABLE [dbo].[ModelData] CHECK CONSTRAINT [FK_ModelData_ClmTask]
end else begin
	print 'Table [dbo].[ModelData] already exists ...'
end
go




IF OBJECT_ID('[dbo].[ResultData]') IS NULL begin
	print 'Creating table [dbo].[ResultData] ...'

	CREATE TABLE [dbo].[ResultData](
		[resultDataId] [bigint] IDENTITY(1,1) NOT NULL,
		[modelDataId] [bigint] NOT NULL,
		[y0] [money] NOT NULL,
		[tEnd] [money] NOT NULL,
		[useAbundant] [bit] NOT NULL DEFAULT ((0)),
		[maxEe] [float] NOT NULL DEFAULT ((0)),
		[maxAverageEe] [float] NOT NULL DEFAULT ((0)),
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




IF OBJECT_ID('[dbo].[RunQueue]') IS NULL begin
	print 'Creating table [dbo].[RunQueue] ...'

	CREATE TABLE [dbo].[RunQueue](
		[runQueueId] [bigint] IDENTITY(1,1) NOT NULL,
		[modelDataId] [bigint] NOT NULL,
		[y0] [money] NOT NULL,
		[tEnd] [money] NOT NULL,
		[useAbundant] [bit] NOT NULL DEFAULT ((0)),
		[statusId] [int] NOT NULL DEFAULT ((0)),
		[createdOn] [datetime] NOT NULL DEFAULT (getdate()),
	 CONSTRAINT [PK_RunQueue] PRIMARY KEY CLUSTERED 
	(
		[runQueueId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]
end else begin
	print 'Table [dbo].[RunQueue] already exists ...'
end
go



