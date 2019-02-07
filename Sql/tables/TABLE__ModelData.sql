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




