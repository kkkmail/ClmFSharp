IF NOT EXISTS(SELECT * FROM INFORMATION_SCHEMA.COLUMNS WHERE  TABLE_NAME = 'ClmTask' AND COLUMN_NAME = 'statusId')
BEGIN
	print 'Adding column statusId to table ClmTask.'
	ALTER TABLE ClmTask ADD statusId int not null default (0)
END
else begin
	print 'Table ClmTask already has column statusId.'
end
go


