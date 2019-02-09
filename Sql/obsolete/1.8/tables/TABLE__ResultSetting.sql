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



