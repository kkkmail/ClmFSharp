CREATE TABLE [dbo].[Setting](
	[settingId] [uniqueidentifier] ROWGUIDCOL  NOT NULL,
	[settingField1] [nvarchar](50) NOT NULL,
	[settingOrderId1] [int] NOT NULL,
	[settingField2] [nvarchar](50) NOT NULL,
	[settingOrderId2] [int] NOT NULL,
	[settingField3] [nvarchar](50) NOT NULL,
	[settingOrderId3] [int] NOT NULL,
	[settingField4] [nvarchar](50) NOT NULL,
	[settingOrderId4] [int] NOT NULL,
	[settingField5] [nvarchar](50) NOT NULL,
	[settingOrderId5] [int] NOT NULL,
	[settingField6] [nvarchar](50) NOT NULL,
	[settingOrderId6] [int] NOT NULL,
	[settingField7] [nvarchar](50) NOT NULL,
	[settingOrderId7] [int] NOT NULL,
	[settingField8] [nvarchar](50) NOT NULL,
	[settingOrderId8] [int] NOT NULL,
	[settingField9] [nvarchar](50) NOT NULL,
	[settingOrderId9] [int] NOT NULL,
	[settingField10] [nvarchar](50) NOT NULL,
	[settingOrderId10] [int] NOT NULL,
	[settingBit] [bit] NOT NULL,
	[settingLong] [bigint] NOT NULL,
	[settingMoney] [money] NOT NULL,
	[settingFloat] [float] NOT NULL,
	[settingDate] [datetime] NULL,
	[settingText] [nvarchar](1000) NULL,
	[settingMemo] [nvarchar](max) NULL,
	[settingGUID] [uniqueidentifier] NULL,
 CONSTRAINT [PK_Setting] PRIMARY KEY CLUSTERED 
(
	[settingId] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

ALTER TABLE [dbo].[Setting] ADD  CONSTRAINT [DF_Setting_settingId]  DEFAULT (newid()) FOR [settingId]
GO

ALTER TABLE [dbo].[Setting] ADD  CONSTRAINT [DF_Setting_settingField1]  DEFAULT ((0)) FOR [settingField1]
GO

ALTER TABLE [dbo].[Setting] ADD  CONSTRAINT [DF_Setting_settingOrderId1]  DEFAULT ((0)) FOR [settingOrderId1]
GO

ALTER TABLE [dbo].[Setting] ADD  CONSTRAINT [DF_Setting_settingOrderId2]  DEFAULT ((0)) FOR [settingOrderId2]
GO

ALTER TABLE [dbo].[Setting] ADD  CONSTRAINT [DF_Setting_settingOrderId3]  DEFAULT ((0)) FOR [settingOrderId3]
GO

ALTER TABLE [dbo].[Setting] ADD  CONSTRAINT [DF_Setting_settingOrderId4]  DEFAULT ((0)) FOR [settingOrderId4]
GO

ALTER TABLE [dbo].[Setting] ADD  CONSTRAINT [DF_Setting_settingOrderId5]  DEFAULT ((0)) FOR [settingOrderId5]
GO

ALTER TABLE [dbo].[Setting] ADD  CONSTRAINT [DF_Setting_settingOrderId6]  DEFAULT ((0)) FOR [settingOrderId6]
GO

ALTER TABLE [dbo].[Setting] ADD  CONSTRAINT [DF_Setting_settingOrderId7]  DEFAULT ((0)) FOR [settingOrderId7]
GO

ALTER TABLE [dbo].[Setting] ADD  CONSTRAINT [DF_Setting_settingOrderId8]  DEFAULT ((0)) FOR [settingOrderId8]
GO

ALTER TABLE [dbo].[Setting] ADD  CONSTRAINT [DF_Setting_settingOrderId9]  DEFAULT ((0)) FOR [settingOrderId9]
GO

ALTER TABLE [dbo].[Setting] ADD  CONSTRAINT [DF_Setting_settingOrderId10]  DEFAULT ((0)) FOR [settingOrderId10]
GO

ALTER TABLE [dbo].[Setting] ADD  CONSTRAINT [DF_Setting_settingBit]  DEFAULT ((0)) FOR [settingBit]
GO

ALTER TABLE [dbo].[Setting] ADD  CONSTRAINT [DF_Setting_settingLong]  DEFAULT ((0)) FOR [settingLong]
GO

ALTER TABLE [dbo].[Setting] ADD  CONSTRAINT [DF_Setting_settingMoney]  DEFAULT ((0)) FOR [settingMoney]
GO

ALTER TABLE [dbo].[Setting] ADD  CONSTRAINT [DF_Setting_settingFloat]  DEFAULT ((0)) FOR [settingFloat]
GO


