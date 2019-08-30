IF OBJECT_ID('[dbo].[CommandLineParam]') IS NULL begin
	print 'Creating table [dbo].[CommandLineParam] ...'

	CREATE TABLE [dbo].[CommandLineParam](
		[commandLineParamId] [uniqueidentifier] NOT NULL,
		[modelDataId] [uniqueidentifier] NOT NULL,
		[y0] [money] NOT NULL,
		[tEnd] [money] NOT NULL,
		[useAbundant] [bit] NOT NULL DEFAULT ((0)),
		[createdOn] datetime not null default ((getdate())),
	 CONSTRAINT [PK_TCommandLineParam] PRIMARY KEY CLUSTERED 
	(
		commandLineParamId ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]

	ALTER TABLE [dbo].[CommandLineParam]  WITH CHECK ADD  CONSTRAINT [FK_CommandLineParam_ModelData] FOREIGN KEY([modelDataId])
	REFERENCES [dbo].ModelData ([modelDataId])

	ALTER TABLE [dbo].[CommandLineParam] CHECK CONSTRAINT [FK_CommandLineParam_ModelData]
end else begin
	print 'Table [dbo].[CommandLineParam] already exists ...'
end
go



