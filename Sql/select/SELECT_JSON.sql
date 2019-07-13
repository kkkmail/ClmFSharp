declare @json nvarchar(max)
set @json = '{"rateParams":[{"Case":"WasteRecyclingRateParam","Fields":[{"wasteRecyclingRate":0.1}]},{"Case":"SynthesisRateParam","Fields":[{"Case":"SynthRndParam","Fields":[{"synthesisDistribution":{"Case":"Distribution","Fields":[{"distributionType":{"Case":"Delta"},"distributionParams":{"threshold":null,"scale":null,"shift":{"Case":"Some","Fields":[1.0]}}}]},"forwardScale":{"Case":"Some","Fields":[0.001]},"backwardScale":null}]}]},{"Case":"CatalyticSynthesisRateParam","Fields":[{"Case":"CatSynthSimParam","Fields":[{"catSynthParam":{"synthesisParam":{"Case":"SynthRndParam","Fields":[{"synthesisDistribution":{"Case":"Distribution","Fields":[{"distributionType":{"Case":"Delta"},"distributionParams":{"threshold":null,"scale":null,"shift":{"Case":"Some","Fields":[1.0]}}}]},"forwardScale":{"Case":"Some","Fields":[0.001]},"backwardScale":null}]},"catSynthRndEeParams":{"rateMultiplierDistr":{"Case":"RateMultDistr","Fields":[{"Case":"Distribution","Fields":[{"distributionType":{"Case":"Triangular"},"distributionParams":{"threshold":{"Case":"Some","Fields":[1E-05]},"scale":{"Case":"Some","Fields":[100000.0]},"shift":null}}]}]},"eeForwardDistribution":{"Case":"Some","Fields":[{"Case":"EeDistribution","Fields":[{"Case":"Distribution","Fields":[{"distributionType":{"Case":"BiDelta"},"distributionParams":{"threshold":null,"scale":{"Case":"Some","Fields":[0.95]},"shift":null}}]}]}]},"eeBackwardDistribution":null}},"catSynthSimParam":{"simBaseDistribution":{"Case":"Distribution","Fields":[{"distributionType":{"Case":"Uniform"},"distributionParams":{"threshold":{"Case":"Some","Fields":[0.2]},"scale":null,"shift":{"Case":"Some","Fields":[1.0]}}}]},"getRateMultiplierDistr":{"Case":"DeltaRateMultDistrGetter"},"getForwardEeDistr":{"Case":"DeltaEeDistributionGetter"},"getBackwardEeDistr":{"Case":"DeltaEeDistributionGetter"}}}]}]},{"Case":"DestructionRateParam","Fields":[{"Case":"DestrRndParam","Fields":[{"destructionDistribution":{"Case":"Distribution","Fields":[{"distributionType":{"Case":"Delta"},"distributionParams":{"threshold":null,"scale":null,"shift":{"Case":"Some","Fields":[1.0]}}}]},"forwardScale":{"Case":"Some","Fields":[0.001]},"backwardScale":null}]}]},{"Case":"CatalyticDestructionRateParam","Fields":[{"Case":"CatDestrSimParam","Fields":[{"catDestrSimParam":{"simBaseDistribution":{"Case":"Distribution","Fields":[{"distributionType":{"Case":"Uniform"},"distributionParams":{"threshold":{"Case":"Some","Fields":[0.2]},"scale":null,"shift":{"Case":"Some","Fields":[1.0]}}}]},"getRateMultiplierDistr":{"Case":"DeltaRateMultDistrGetter"},"getForwardEeDistr":{"Case":"DeltaEeDistributionGetter"},"getBackwardEeDistr":{"Case":"DeltaEeDistributionGetter"}},"catDestrParam":{"catDestrRndEeParams":{"rateMultiplierDistr":{"Case":"RateMultDistr","Fields":[{"Case":"Distribution","Fields":[{"distributionType":{"Case":"Triangular"},"distributionParams":{"threshold":{"Case":"Some","Fields":[1E-05]},"scale":{"Case":"Some","Fields":[100000.0]},"shift":null}}]}]},"eeForwardDistribution":{"Case":"Some","Fields":[{"Case":"EeDistribution","Fields":[{"Case":"Distribution","Fields":[{"distributionType":{"Case":"BiDelta"},"distributionParams":{"threshold":null,"scale":{"Case":"Some","Fields":[0.95]},"shift":null}}]}]}]},"eeBackwardDistribution":null},"destructionParam":{"Case":"DestrRndParam","Fields":[{"destructionDistribution":{"Case":"Distribution","Fields":[{"distributionType":{"Case":"Delta"},"distributionParams":{"threshold":null,"scale":null,"shift":{"Case":"Some","Fields":[1.0]}}}]},"forwardScale":{"Case":"Some","Fields":[0.001]},"backwardScale":null}]}}}]}]},{"Case":"LigationRateParam","Fields":[{"Case":"LigRndParam","Fields":[{"ligationDistribution":{"Case":"Distribution","Fields":[{"distributionType":{"Case":"Delta"},"distributionParams":{"threshold":null,"scale":null,"shift":{"Case":"Some","Fields":[1.0]}}}]},"forwardScale":{"Case":"Some","Fields":[1.0]},"backwardScale":{"Case":"Some","Fields":[1.0]}}]}]}]}'

; with tbl1 as
(
select 
	 y.[key] as yKey
	,z.*
	--,a.*
from openjson(@json) x
cross apply openjson(x.[value]) as y
cross apply openjson(y.[value]) as z
where  z.[key] = 'Fields'
)
select b.[value]
from tbl1
cross apply openjson(tbl1.[value]) as a
cross apply openjson(a.[value]) as b
where b.[key] = 'wasteRecyclingRate'

--select json_value(@json, '$.rateParams.ArrayValue[0]')
--select json_query(@json, '$.rateParams.ArrayValue[0]')

--select json_value(@json, '$.rateParams[0]')

--select 1 as id
--cross apply openjson(json_query(@json, '$.rateParams')) as x
--cross apply openjson(x.[value], '$') as y

--select json_value(@json, '$.rateParams[0].Case')
--select json_query(@json, '$.rateParams[0].Case')

--select * 
--from dbo.JSONHierarchy(@json, DEFAULT, DEFAULT, DEFAULT)
--where ValueType <> 'array'

