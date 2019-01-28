CREATE TABLE [dbo].[RunQueue](
	[runQueueId] [bigint] IDENTITY(1,1) NOT NULL,
	[modelDataId] [bigint] NOT NULL,
	[y0] [money] NOT NULL,
	[tEnd] [money] NOT NULL,
	[useAbundant] [bit] NOT NULL,
	[statusId] [int] NOT NULL,
 CONSTRAINT [PK_RunQueue] PRIMARY KEY CLUSTERED 
(
	[runQueueId] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[RunQueue] ADD  CONSTRAINT [DF_RunQueue_useAbundant]  DEFAULT ((0)) FOR [useAbundant]
GO

ALTER TABLE [dbo].[RunQueue] ADD  CONSTRAINT [DF_RunQueue_statusId]  DEFAULT ((0)) FOR [statusId]
GO


