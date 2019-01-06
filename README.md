# ClmFSharp
F# modelling of chiral symmetry breaking in chemical systems.
The system core consists of a model generator, which generates a model file, ModelData.fs using preconfigured statistical distributions, model types, and some other parameters. 

Version 1.5 introduces "continuous model generation" where a generator loads parameters from the database generates the model, compiles it and then schedules it to run for all configured initial parameters. That keeps processing load near 100% for as long as the generator runs. The parameters can be adjusted on the fly and that will affect the models, which are generated after that.
