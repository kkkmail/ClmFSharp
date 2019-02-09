IF OBJECT_ID('[dbo].[ModelData]') IS NULL begin
	print 'Creating table [dbo].[ModelData] ...'

	CREATE TABLE [dbo].[ModelData](
		[modelDataId] [bigint] IDENTITY(1,1) NOT NULL,
		[numberOfAminoAcids] [int] NOT NULL,
		[maxPeptideLength] [int] NOT NULL,
		[defaultSetIndex] [int] NOT NULL DEFAULT ((-1)),
		[fileStructureVersion] [nvarchar](50) NOT NULL,
		[seedValue] [int] NULL,
		[modelDataParams] [nvarchar](max) NOT NULL,
		[modelBinaryData] [varbinary](max) NOT NULL,
		[createdOn] [datetime] NOT NULL DEFAULT (getdate()),
	 CONSTRAINT [PK_ModelData] PRIMARY KEY CLUSTERED 
	(
		[modelDataId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
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
	 CONSTRAINT [PK_RunQueue] PRIMARY KEY CLUSTERED 
	(
		[runQueueId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]
end else begin
	print 'Table [dbo].[RunQueue] already exists ...'
end
go



IF OBJECT_ID('[dbo].[Setting]') IS NULL begin
	print 'Creating table [dbo].[Setting] ...'

	CREATE TABLE [dbo].[Setting](
		[settingId] [int] NOT NULL DEFAULT (0),
		[settings] [nvarchar](max) NOT NULL,
	 CONSTRAINT [PK_Setting] PRIMARY KEY CLUSTERED 
	(
		[settingId] ASC
	),
	constraint CK_Setting CHECK ([settingId] = 0)
	)
end else begin
	print 'Table [dbo].[Setting] already exists ...'
end
go


