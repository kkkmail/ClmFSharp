IF OBJECT_ID('[dbo].[ClmDefaultValue]') IS NULL begin
	print 'Creating table [dbo].[ClmDefaultValue] ...'

	CREATE TABLE [dbo].[ClmDefaultValue](
		clmDefaultValueId [bigint] NOT NULL,
		[defaultRateParams] [nvarchar](max) NOT NULL,
		[description] nvarchar(2000) NULL,
		[fileStructureVersion] money NOT NULL,
	 CONSTRAINT [PK_ClmDefaultValue] PRIMARY KEY CLUSTERED 
	(
		clmDefaultValueId ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
end else begin
	print 'Table [dbo].[ClmDefaultValue] already exists ...'
end
go


IF OBJECT_ID('[dbo].[ClmTask]') IS NULL begin
	print 'Creating table [dbo].[ClmTask] ...'

	CREATE TABLE [dbo].[ClmTask](
		[clmTaskId] [uniqueidentifier] NOT NULL,
		[clmDefaultValueId] [bigint] NOT NULL,
		[numberOfAminoAcids] [int] NOT NULL,
		[maxPeptideLength] [int] NOT NULL,
		[numberOfRepetitions] [int] NOT NULL DEFAULT ((1)),
		[remainingRepetitions] [int] NOT NULL DEFAULT ((1)),
		[statusId] [int] NOT NULL DEFAULT ((0)),
		[createdOn] [datetime] NOT NULL DEFAULT (getdate()),
	 CONSTRAINT [PK_ClmTask] PRIMARY KEY CLUSTERED 
	(
		[clmTaskId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]
end else begin
	print 'Table [dbo].[ClmTask] already exists ...'
end
go



IF OBJECT_ID('[dbo].[CommandLineParam]') IS NULL begin
	print 'Creating table [dbo].[CommandLineParam] ...'

	CREATE TABLE [dbo].[CommandLineParam](
		commandLineParamId [uniqueidentifier] NOT NULL,
		[clmTaskId] [uniqueidentifier] NOT NULL,
		[y0] [money] NOT NULL,
		[tEnd] [money] NOT NULL,
		[useAbundant] [bit] NOT NULL DEFAULT ((0)),
		[createdOn] datetime not null default ((getdate())),
	 CONSTRAINT [PK_TCommandLineParam] PRIMARY KEY CLUSTERED 
	(
		commandLineParamId ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]

	ALTER TABLE [dbo].[CommandLineParam]  WITH CHECK ADD  CONSTRAINT [FK_CommandLineParam_ClmTask] FOREIGN KEY([clmTaskId])
	REFERENCES [dbo].ClmTask ([clmTaskId])

	ALTER TABLE [dbo].[CommandLineParam] CHECK CONSTRAINT [FK_CommandLineParam_ClmTask]
end else begin
	print 'Table [dbo].[CommandLineParam] already exists ...'
end
go



IF OBJECT_ID('[dbo].[ModelData]') IS NULL begin
	print 'Creating table [dbo].[ModelData] ...'

	CREATE TABLE [dbo].[ModelData](
		[modelDataId] [uniqueidentifier] NOT NULL,
		[clmTaskId] [uniqueidentifier] NOT NULL,
		[parentModelDataId] [uniqueidentifier] NULL,
		[fileStructureVersion] money NOT NULL,
		[seedValue] [int] NULL,
		[modelDataParams] [nvarchar](max) NOT NULL,
		[modelBinaryData] [varbinary](max) NOT NULL,
		[createdOn] [datetime] NOT NULL DEFAULT (getdate()),
	 CONSTRAINT [PK_ModelData] PRIMARY KEY CLUSTERED 
	(
		[modelDataId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

	ALTER TABLE [dbo].[ModelData]  WITH CHECK ADD CONSTRAINT [FK_ModelData_ClmTask] FOREIGN KEY([clmTaskId])
	REFERENCES [dbo].ClmTask ([clmTaskId])

	ALTER TABLE [dbo].[ModelData] CHECK CONSTRAINT [FK_ModelData_ClmTask]

	ALTER TABLE [dbo].[ModelData]  WITH CHECK ADD CONSTRAINT [FK_ModelData_ModelData] FOREIGN KEY([parentModelDataId])
	REFERENCES [dbo].[ModelData] ([modelDataId])

	ALTER TABLE [dbo].[ModelData] CHECK CONSTRAINT [FK_ModelData_ModelData]
end else begin
	print 'Table [dbo].[ModelData] already exists ...'
end
go




IF OBJECT_ID('[dbo].[ResultData]') IS NULL begin
	print 'Creating table [dbo].[ResultData] ...'

	CREATE TABLE [dbo].[ResultData](
		[resultDataId] [uniqueidentifier] NOT NULL,
		[workerNodeId] [uniqueidentifier] NOT NULL,
		[modelDataId] [uniqueidentifier] NOT NULL,
		[y0] [money] NOT NULL,
		[tEnd] [money] NOT NULL,
		[useAbundant] [bit] NOT NULL DEFAULT ((0)),
		[maxEe] [float] NOT NULL DEFAULT ((0)),
		[maxAverageEe] [float] NOT NULL DEFAULT ((0)),
		[maxWeightedAverageAbsEe] [float] NOT NULL DEFAULT ((0)),
		[maxLastEe] [float] NOT NULL DEFAULT ((0)),
		[createdOn] [datetime] NOT NULL DEFAULT (getdate()),
	 CONSTRAINT [PK_ResultData] PRIMARY KEY CLUSTERED 
	(
		[resultDataId] ASC
	)
	)

	ALTER TABLE [dbo].[ResultData]  WITH CHECK ADD  CONSTRAINT [FK_ResultData_ResultData] FOREIGN KEY([modelDataId])
	REFERENCES [dbo].[ModelData] ([modelDataId])

	ALTER TABLE [dbo].[ResultData] CHECK CONSTRAINT [FK_ResultData_ResultData]
end else begin
	print 'Table [dbo].[ResultData] already exists ...'
end
go




IF OBJECT_ID('[dbo].[RunQueue]') IS NULL begin
	print 'Creating table [dbo].[RunQueue] ...'

	CREATE TABLE [dbo].[RunQueue](
		[runQueueId] [uniqueidentifier] NOT NULL,
		[modelDataId] [uniqueidentifier] NOT NULL,
		[y0] [money] NOT NULL,
		[tEnd] [money] NOT NULL,
		[useAbundant] [bit] NOT NULL DEFAULT ((0)),
		[statusId] [int] NOT NULL DEFAULT ((0)),
		[createdOn] [datetime] NOT NULL DEFAULT (getdate()),
	 CONSTRAINT [PK_RunQueue] PRIMARY KEY CLUSTERED 
	(
		[runQueueId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]
end else begin
	print 'Table [dbo].[RunQueue] already exists ...'
end
go



drop function if exists dbo.getCatDestrScarcity
go

create function dbo.getCatDestrScarcity(@clmDefaultValueId bigint)
returns float
as
begin
	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from ClmDefaultValue where clmDefaultValueId = @clmDefaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.* 
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select b.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'catDestrParam'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'catDestrRndEeParams'
	)
	,t5 as
	(
		select a.*
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'rateMultiplierDistr'
	)
	,t6 as
	(
		select a.* 
		from t5
		cross apply openjson(t5.[value]) as a
		where a.[key] = 'Fields'
	)
	,t7 as
	(
		select b.*
		from t6
		cross apply openjson(t6.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t8 as
	(
		select b.* 
		from t7
		cross apply openjson(t7.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'distributionParams'
	)
	,t9 as
	(
		select a.* 
		from t8
		cross apply openjson(t8.[value]) as a
		where a.[key] = 'threshold'
	)
	,t10 as
	(
		select a.* 
		from t9
		cross apply openjson(t9.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float) * 1.0E06
	from t10
	cross apply openjson(t10.[value]) as a

	return @retval
end
go
--declare @clmDefaultValueId bigint
--set @clmDefaultValueId = 9028

drop function if exists dbo.getCatDestrSim
go

create function dbo.getCatDestrSim(@clmDefaultValueId bigint)
returns float
as
begin
	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from ClmDefaultValue where clmDefaultValueId = @clmDefaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.* 
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select b.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'catDestrSimParam'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'catRatesSimGeneration'
	)
	,t5 as
	(
		select a.*
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'Fields'
	)
	,t6 as
	(
		select d.*
		from t5
		cross apply openjson(t5.[value]) as a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		cross apply openjson(c.[value]) as d
		where b.[key] = 'Fields' and d.[key] = 'distributionParams'
	)
	,t7 as
	(
		select a.*
		from t6
		cross apply openjson(t6.[value]) as a
		where a.[key] = 'threshold'
	)
	,t8 as
	(
		select a.* 
		from t7
		cross apply openjson(t7.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float)
	from t8
	cross apply openjson(t8.[value]) as a

	return @retval
end
go
drop function if exists dbo.getCatSynthScarcity
go

create function dbo.getCatSynthScarcity(@clmDefaultValueId bigint)
returns float
as
begin
	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from ClmDefaultValue where clmDefaultValueId = @clmDefaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.* 
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select b.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'catSynthParam'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'catSynthRndEeParams'
	)
	,t5 as
	(
		select a.*
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'rateMultiplierDistr'
	)
	,t6 as
	(
		select a.* 
		from t5
		cross apply openjson(t5.[value]) as a
		where a.[key] = 'Fields'
	)
	,t7 as
	(
		select b.*
		from t6
		cross apply openjson(t6.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t8 as
	(
		select b.* 
		from t7
		cross apply openjson(t7.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'distributionParams'
	)
	,t9 as
	(
		select a.* 
		from t8
		cross apply openjson(t8.[value]) as a
		where a.[key] = 'threshold'
	)
	,t10 as
	(
		select a.* 
		from t9
		cross apply openjson(t9.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float) * 1.0E06
	from t10
	cross apply openjson(t10.[value]) as a

	return @retval
end
go
--declare @clmDefaultValueId bigint
--set @clmDefaultValueId = 9028

drop function if exists dbo.getCatSynthSim
go

create function dbo.getCatSynthSim(@clmDefaultValueId bigint)
returns float
as
begin
	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from ClmDefaultValue where clmDefaultValueId = @clmDefaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	,t2 as
	(
		select b.* 
		from t1
		cross apply openjson(t1.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'Fields'
	)
	,t3 as
	(
		select b.* 
		from t2
		cross apply openjson(t2.[value]) as a
		cross apply openjson(a.[value]) as b
		where b.[key] = 'catSynthSimParam'
	)
	,t4 as
	(
		select a.* 
		from t3
		cross apply openjson(t3.[value]) as a
		where a.[key] = 'catRatesSimGeneration'
	)
	,t5 as
	(
		select a.*
		from t4
		cross apply openjson(t4.[value]) as a
		where a.[key] = 'Fields'
	)
	,t6 as
	(
		select d.*
		from t5
		cross apply openjson(t5.[value]) as a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		cross apply openjson(c.[value]) as d
		where b.[key] = 'Fields' and d.[key] = 'distributionParams'
	)
	,t7 as
	(
		select a.*
		from t6
		cross apply openjson(t6.[value]) as a
		where a.[key] = 'threshold'
	)
	,t8 as
	(
		select a.* 
		from t7
		cross apply openjson(t7.[value]) as a
		where a.[key] = 'Fields'
	)
	select @retval = cast(a.[value] as float)
	from t8
	cross apply openjson(t8.[value]) as a

	return @retval
end
go
drop function if exists dbo.getGroupId
go

create function dbo.getGroupId(@clmDefaultValueId bigint)
returns bigint
as
begin
	return (@clmDefaultValueId / 1000000000)
end
go
drop function if exists dbo.getWasteRecyclingRate
go

create function dbo.getWasteRecyclingRate(@clmDefaultValueId bigint)
returns float
as
begin
	declare @json nvarchar(max), @retVal float
	select @json = defaultRateparams from ClmDefaultValue where clmDefaultValueId = @clmDefaultValueId

	;with t1 as
	(
		select 
			 b.[key] as yKey
			,c.*
		from openjson(@json) a
		cross apply openjson(a.[value]) as b
		cross apply openjson(b.[value]) as c
		where a.[key] = 'rateParams' and c.[key] = 'Fields'
	)
	select @retVal = cast(b.[value] as float)
	from t1
	cross apply openjson(t1.[value]) as a
	cross apply openjson(a.[value]) as b
	where b.[key] = 'wasteRecyclingRate'

	return @retVal
end
go
drop function if exists dbo.JSONHierarchy
go

-- https://www.red-gate.com/simple-talk/blogs/consuming-hierarchical-json-documents-sql-server-using-openjson/
CREATE FUNCTION dbo.JSONHierarchy
  (
  @JSONData NVARCHAR(MAX),
  @Parent_object_ID INT = NULL,
  @MaxObject_id INT = 0,
  @type INT = null
  )
RETURNS @ReturnTable TABLE
  (
  Element_ID INT IDENTITY(1, 1) PRIMARY KEY, /* internal surrogate primary key gives the order of parsing and the list order */
  SequenceNo INT NULL, /* the sequence number in a list */
  Parent_ID INT, /* if the element has a parent then it is in this column. The document is the ultimate parent, so you can get the structure from recursing from the document */
  Object_ID INT, /* each list or object has an object id. This ties all elements to a parent. Lists are treated as objects here */
  Name NVARCHAR(2000), /* the name of the object */
  StringValue NVARCHAR(MAX) NOT NULL, /*the string representation of the value of the element. */
  ValueType NVARCHAR(10) NOT NULL /* the declared type of the value represented as a string in StringValue*/
  )
AS
  BEGIN
	--the types of JSON
    DECLARE @null INT =
      0, @string INT = 1, @int INT = 2, @boolean INT = 3, @array INT = 4, @object INT = 5;
 
    DECLARE @OpenJSONData TABLE
      (
      sequence INT IDENTITY(1, 1),
      [key] NVARCHAR(200),
      Value NVARCHAR(MAX),
      type INT
      );
 
    DECLARE @key NVARCHAR(200), @Value NVARCHAR(MAX), @Thetype INT, @ii INT, @iiMax INT,
      @NewObject INT, @firstchar CHAR(1);
 
    INSERT INTO @OpenJSONData
      ([key], Value, type)
      SELECT [Key], Value, Type FROM OpenJson(@JSONData);
	SELECT @ii = 1, @iiMax = Scope_Identity()
    SELECT  @Firstchar= --the first character to see if it is an object or an array
	  Substring(@JSONData,PatIndex('%[^'+CHAR(0)+'- '+CHAR(160)+']%',' '+@JSONData+'!' collate SQL_Latin1_General_CP850_Bin)-1,1)
    IF @type IS NULL AND @firstchar IN ('[','{')
		begin
	   INSERT INTO @returnTable
	    (SequenceNo,Parent_ID,Object_ID,Name,StringValue,ValueType)
			SELECT 1,NULL,1,'-','', 
			   CASE @firstchar WHEN '[' THEN 'array' ELSE 'object' END
        SELECT @type=CASE @firstchar WHEN '[' THEN @array ELSE @object END,
		@Parent_object_ID  = 1, @MaxObject_id=Coalesce(@MaxObject_id, 1) + 1;
		END       
	WHILE(@ii <= @iiMax)
      BEGIN
	  --OpenJSON renames list items with 0-nn which confuses the consumers of the table
        SELECT @key = CASE WHEN [key] LIKE '[0-9]%' THEN NULL ELSE [key] end , @Value = Value, @Thetype = type
          FROM @OpenJSONData
          WHERE sequence = @ii;
 
        IF @Thetype IN (@array, @object) --if we have been returned an array or object
          BEGIN
            SELECT @MaxObject_id = Coalesce(@MaxObject_id, 1) + 1;
			--just in case we have an object or array returned
            INSERT INTO @ReturnTable --record the object itself
              (SequenceNo, Parent_ID, Object_ID, Name, StringValue, ValueType)
              SELECT @ii, @Parent_object_ID, @MaxObject_id, @key, '',
                CASE @Thetype WHEN @array THEN 'array' ELSE 'object' END;
 
            INSERT INTO @ReturnTable --and return all its children
              (SequenceNo, Parent_ID, Object_ID, [Name],  StringValue, ValueType)
			  SELECT SequenceNo, Parent_ID, Object_ID, 
				[Name],
				Coalesce(StringValue,'null'),
				ValueType
              FROM dbo.JSONHierarchy(@Value, @MaxObject_id, @MaxObject_id, @type);
			SELECT @MaxObject_id=Max(Object_id)+1 FROM @ReturnTable
		  END;
        ELSE
          INSERT INTO @ReturnTable
            (SequenceNo, Parent_ID, Object_ID, Name, StringValue, ValueType)
            SELECT @ii, @Parent_object_ID, NULL, @key, Coalesce(@Value,'null'),
              CASE @Thetype WHEN @string THEN 'string'
                WHEN @null THEN 'null'
                WHEN @int THEN 'int'
                WHEN @boolean THEN 'boolean' ELSE 'int' END;
 
        SELECT @ii = @ii + 1;
      END;
 
    RETURN;
  END;
GO
