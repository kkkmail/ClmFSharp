IF OBJECT_ID('[dbo].[ModelData]') IS NULL begin
	print 'Creating table [dbo].[ModelData] ...'

	CREATE TABLE [dbo].[ModelData](
		[modelDataId] [bigint] IDENTITY(1,1) NOT NULL,
		[numberOfAminoAcids] [int] NOT NULL,
		[maxPeptideLength] [int] NOT NULL,
		[seedValue] [int] NULL,
		[defaultSetIndex] [int] NOT NULL DEFAULT ((-1)),
		[fileStructureVersion] [nvarchar](50) NOT NULL,
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




IF OBJECT_ID('[dbo].[ModelSetting]') IS NULL begin
	print 'Creating table [dbo].[ModelSetting] ...'

	CREATE TABLE [dbo].[ModelSetting](
		[modelSettingId] [uniqueidentifier] ROWGUIDCOL NOT NULL DEFAULT (newid()),
		[modelDataId] [bigint] NOT NULL,
		[settingField1] [nvarchar](50) NOT NULL,
		[settingOrderId1] [int] NOT NULL DEFAULT ((0)),
		[settingField2] [nvarchar](50) NOT NULL,
		[settingOrderId2] [int] NOT NULL DEFAULT ((0)),
		[settingField3] [nvarchar](50) NOT NULL,
		[settingOrderId3] [int] NOT NULL DEFAULT ((0)),
		[settingField4] [nvarchar](50) NOT NULL,
		[settingOrderId4] [int] NOT NULL DEFAULT ((0)),
		[settingField5] [nvarchar](50) NOT NULL,
		[settingOrderId5] [int] NOT NULL DEFAULT ((0)),
		[settingField6] [nvarchar](50) NOT NULL,
		[settingOrderId6] [int] NOT NULL DEFAULT ((0)),
		[settingField7] [nvarchar](50) NOT NULL,
		[settingOrderId7] [int] NOT NULL DEFAULT ((0)),
		[settingField8] [nvarchar](50) NOT NULL,
		[settingOrderId8] [int] NOT NULL DEFAULT ((0)),
		[settingField9] [nvarchar](50) NOT NULL,
		[settingOrderId9] [int] NOT NULL DEFAULT ((0)),
		[settingField10] [nvarchar](50) NOT NULL,
		[settingOrderId10] [int] NOT NULL DEFAULT ((0)),
		[settingBit] [bit] NOT NULL DEFAULT ((0)),
		[settingLong] [bigint] NOT NULL DEFAULT ((0)),
		[settingMoney] [money] NOT NULL DEFAULT ((0)),
		[settingFloat] [float] NOT NULL DEFAULT ((0)),
		[settingDate] [datetime] NULL,
		[settingText] [nvarchar](1000) NULL,
		[settingMemo] [nvarchar](max) NULL,
		[settingGUID] [uniqueidentifier] NULL,
	 CONSTRAINT [PK_ModelSetting] PRIMARY KEY CLUSTERED 
	(
		[modelSettingId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]


	ALTER TABLE [dbo].[ModelSetting]  WITH CHECK ADD  CONSTRAINT [FK_ModelSetting_ModelData] FOREIGN KEY([modelDataId])
	REFERENCES [dbo].[ModelData] ([modelDataId])

	ALTER TABLE [dbo].[ModelSetting] CHECK CONSTRAINT [FK_ModelSetting_ModelData]
end else begin
	print 'Table [dbo].[ModelSetting] already exists ...'
end
go




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




IF OBJECT_ID('[dbo].[ResultSetting]') IS NULL begin
	print 'Creating table [dbo].[ResultSetting] ...'

	CREATE TABLE [dbo].[ResultSetting](
		[resultSettingId] [uniqueidentifier] ROWGUIDCOL NOT NULL DEFAULT (newid()),
		[resultDataId] [bigint] NOT NULL,
		[settingField1] [nvarchar](50) NOT NULL,
		[settingOrderId1] [int] NOT NULL DEFAULT ((0)),
		[settingField2] [nvarchar](50) NOT NULL,
		[settingOrderId2] [int] NOT NULL DEFAULT ((0)),
		[settingField3] [nvarchar](50) NOT NULL,
		[settingOrderId3] [int] NOT NULL DEFAULT ((0)),
		[settingField4] [nvarchar](50) NOT NULL,
		[settingOrderId4] [int] NOT NULL DEFAULT ((0)),
		[settingField5] [nvarchar](50) NOT NULL,
		[settingOrderId5] [int] NOT NULL DEFAULT ((0)),
		[settingField6] [nvarchar](50) NOT NULL,
		[settingOrderId6] [int] NOT NULL DEFAULT ((0)),
		[settingField7] [nvarchar](50) NOT NULL,
		[settingOrderId7] [int] NOT NULL DEFAULT ((0)),
		[settingField8] [nvarchar](50) NOT NULL,
		[settingOrderId8] [int] NOT NULL DEFAULT ((0)),
		[settingField9] [nvarchar](50) NOT NULL,
		[settingOrderId9] [int] NOT NULL DEFAULT ((0)),
		[settingField10] [nvarchar](50) NOT NULL,
		[settingOrderId10] [int] NOT NULL DEFAULT ((0)),
		[settingBit] [bit] NOT NULL DEFAULT ((0)),
		[settingLong] [bigint] NOT NULL DEFAULT ((0)),
		[settingMoney] [money] NOT NULL DEFAULT ((0)),
		[settingFloat] [float] NOT NULL DEFAULT ((0)),
		[settingDate] [datetime] NULL,
		[settingText] [nvarchar](1000) NULL,
		[settingMemo] [nvarchar](max) NULL,
		[settingGUID] [uniqueidentifier] NULL,
	 CONSTRAINT [PK_ResultSetting] PRIMARY KEY CLUSTERED 
	(
		[resultSettingId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

	ALTER TABLE [dbo].[ResultSetting]  WITH CHECK ADD  CONSTRAINT [FK_ResultSetting_ResultData] FOREIGN KEY([resultDataId])
	REFERENCES [dbo].[ResultData] ([resultDataId])

	ALTER TABLE [dbo].[ResultSetting] CHECK CONSTRAINT [FK_ResultSetting_ResultData]
end else begin
	print 'Table [dbo].[ResultSetting] already exists ...'
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
		[settingId] [uniqueidentifier] ROWGUIDCOL NOT NULL DEFAULT (newid()),
		[settingField1] [nvarchar](50) NOT NULL,
		[settingOrderId1] [int] NOT NULL DEFAULT ((0)),
		[settingField2] [nvarchar](50) NOT NULL,
		[settingOrderId2] [int] NOT NULL DEFAULT ((0)),
		[settingField3] [nvarchar](50) NOT NULL,
		[settingOrderId3] [int] NOT NULL DEFAULT ((0)),
		[settingField4] [nvarchar](50) NOT NULL,
		[settingOrderId4] [int] NOT NULL DEFAULT ((0)),
		[settingField5] [nvarchar](50) NOT NULL,
		[settingOrderId5] [int] NOT NULL DEFAULT ((0)),
		[settingField6] [nvarchar](50) NOT NULL,
		[settingOrderId6] [int] NOT NULL DEFAULT ((0)),
		[settingField7] [nvarchar](50) NOT NULL,
		[settingOrderId7] [int] NOT NULL DEFAULT ((0)),
		[settingField8] [nvarchar](50) NOT NULL,
		[settingOrderId8] [int] NOT NULL DEFAULT ((0)),
		[settingField9] [nvarchar](50) NOT NULL,
		[settingOrderId9] [int] NOT NULL DEFAULT ((0)),
		[settingField10] [nvarchar](50) NOT NULL,
		[settingOrderId10] [int] NOT NULL DEFAULT ((0)),
		[settingBit] [bit] NOT NULL DEFAULT ((0)),
		[settingLong] [bigint] NOT NULL DEFAULT ((0)),
		[settingMoney] [money] NOT NULL DEFAULT ((0)),
		[settingFloat] [float] NOT NULL DEFAULT ((0)),
		[settingDate] [datetime] NULL,
		[settingText] [nvarchar](1000) NULL,
		[settingMemo] [nvarchar](max) NULL,
		[settingGUID] [uniqueidentifier] NULL,
	 CONSTRAINT [PK_Setting] PRIMARY KEY CLUSTERED 
	(
		[settingId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
end else begin
	print 'Table [dbo].[Setting] already exists ...'
end
go


