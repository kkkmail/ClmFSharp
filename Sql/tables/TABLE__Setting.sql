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


