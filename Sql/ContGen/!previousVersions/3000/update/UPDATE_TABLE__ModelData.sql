IF NOT EXISTS(SELECT * FROM INFORMATION_SCHEMA.COLUMNS WHERE  TABLE_NAME = 'ModelData' AND COLUMN_NAME = 'parentModelDataId')
BEGIN
	print 'Adding column parentModelDataId to table ModelData.'
	ALTER TABLE ModelData ADD parentModelDataId bigint NULL

	ALTER TABLE [dbo].[ModelData]  WITH CHECK ADD CONSTRAINT [FK_ModelData_ModelData] FOREIGN KEY(parentModelDataId)
	REFERENCES [dbo].[ModelData] ([modelDataId])

	ALTER TABLE [dbo].[ModelData] CHECK CONSTRAINT [FK_ModelData_ModelData]
END
else begin
	print 'Table ModelData already has column parentModelDataId.'
end
go


