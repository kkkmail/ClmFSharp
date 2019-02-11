IF OBJECT_ID('[dbo].[AllParams]') IS NULL begin
	print 'Creating table [dbo].[AllParams] ...'

	CREATE TABLE [dbo].[AllParams](
		[allParamsId] [int] NOT NULL DEFAULT (0),
		[allParams] [nvarchar](max) NOT NULL,
	 CONSTRAINT [PK_AllParam] PRIMARY KEY CLUSTERED 
	(
		[allParamsId] ASC
	),
	constraint CK_AllParam CHECK ([allParamsId] = 0)
	)
end else begin
	print 'Table [dbo].[AllParams] already exists ...'
end
go


