# ClmFSharp
## F# modelling of chiral symmetry breaking in chemical systems. Version 3.0.

The main difficulty in modeling organic chemical systems is a very large number of possible reagents and reactions. For example, the number of peptide chains grows as: `(2 * M) * ((2 * M) ^ (N + 1) � 1) / ((2 * M) - 1)` where `M` is the number of considered amino acids and `N` is the maximum considered peptide length. That value for 20 amino acids and maximum peptide length of 3 gives 65,640 possible peptide chains. If some of the reactions can be catalyzed by some of these peptide chains, then, in theory, we must consider that *all* peptide chains can catalyze *all* such reactions, though most of the coefficients are exact zeros. This requires considering huge sparse matrices of coefficients of various reactions. Subsequently, it is not possible to use such matrices directly and all exact zeros must be removed from all equations. To address this problem, we use tuples of (index, value of relevant coefficient) for all non-zero elements. The model parameters control how sparse are the matrices and what are the values of non-zero coefficients. Each model describes a homogeneous chemical system. However, some of the reactions imply a separation of soluble and insoluble substances, which implies inhomogeneous systems.

The system uses MS SQL as a database to store various parameters and generates HTML charts stored in the local folder. All extensive data sets are stored in the database as a JSON and all binary data sets are stored as a zipped JSON. Please, refer to `DbData.DatabaseTypes` for the details. 

Version 3.0 introduced tasks (which are stored in the table `ClmTask`). A task is a combination of some default parameter set (row from `ClmDefaults`), a set of pairs of initial concentration (called `y0` in the system) and a run time (called `tEnd`), and a number of repetitions. Due to statistical nature of the models, it is necessary to repeat the same model many times to achieve statistically valid results. 

The system core consists of a model generator, which generates models using preconfigured statistical distributions based on some default values (table ` ClmDefaultValue` populated from F# project `ClmDefaults` -> `AlDefaults.fs`), model types, and some other parameters. The generator continuously loads parameters from the database, produces models, stores the generated sparse matrices in the database, and then spawns processes to run them for all configured initial values / run time. The number of processes is limited by `Environment.ProcessorCount`. That keeps processing load near 100% for as long as there are any incomplete tasks left. The parameters can be adjusted on the fly and that will affect the models, which are generated after that. Once any model is completed the aggregate results are stored in `ResultData` table. The HTML chats are produced if symmetry breaking occurred in the model.

The components of the system �talk� to each other using WCF. However, since it is a barebone WCF without MEX endpoint exposed, most of the WCF test tools won�t be able to �see� it properly. Due to the time constraint, adding a MEX endpoint does not seem justified at this point because the code works.

## Build order
The system uses F# type providers, which means that the database must be created first. The compile time database name (as well as the run time database name) is currently hard coded in `ClmSys.GeneralData.ClmBaseName` and then reused in `DbData.Configuration.ClmDbName`. Because the database is primitive (it contains less than 10 tables), usage of automated up/down database migrations (like Entity Framework based ones) was not justified. So, the procedure is as follows:
1.	Look up the value of `ClmSys.GeneralData.ClmBaseName` (e.g. `clm3000`) / adjust it as necessary.
2.	Create MSSQL database with the name from step #1.
3.	Run `-build.bat` file from `SQL` folder. It will produce a file `all.sql` in the folder `!All`. If no changes were made, then the file will come out the same as in repository.
4.	Load that file (`all.sql`) and run it in the database created on step #2. The script if fully reentrable, which means that it can be run many times without any side effects.
5.	Once the database is built, F# solution `ContGen.sln` can be loaded and compiled.

## Project and Folder Structure
Folder `Clm` contains F# code. There are two solutions: the main solution `ContGen.sln` and model testing solution `Model.sln`. The fist solution builds the whole system and the second solution is used to perform the primary acceptance test. Given that a properly written F# code is 5-10 times more compact than the equivalent C# code and that, again, a properly written F# code should follow the paradigm: if it compiles, then it works, that removes probably somewhere between 90 to 99 percent of the tests needed to make a working C# code.
Folder `Math` contains various Excel / Wolfram Mathematica helper files and are not needed for the operation of the system.
Folder `SQL` contains the database code and various convenient select statements. 

## Executables and Command Line Parameters
The project `ContGenService` is the primary one and it contains all executables needed for the operation of the system. The system uses `Argu` as a command line parser, so running any of the executables (except `ClmDefaults.exe`, which currently does not have any command line parameters) with `help` command will provide up to date command line parameter structure.

There are currently five executables:
1.	`ClmDefaults.exe` loads / updates current default sets into the database. If any of the default sets are modified or new ones are added in F# code, then this command must be run first. Currently, there are no command line parameters for this command.
2.	`ContGen.exe` is used to add tasks / generate mode code for tests / run specific model �by hands�.
3.	`ContGenService.exe`. This is the primary service. It can be run as a Windows service or as a regular executable.
4.	If `ContGenService.exe` is run as a Windows service, then we need the functionality to control it. This is performed by `ContGenAdm.exe`.
5.	Finally, `SolverRunner.exe` is the ultimate low-lever F# wrapper around ALGLIB vector ODE solver. It solves the given model with given parameters (`y0` and `tEnd`), This is the process that is spawned by `ContGenService.exe` / `ContGen.exe` when a model needs to be run. Running `SolverRunner.exe` �by hands� gives a little bit extra control and allows running a specific model directly. This becomes important when we want to re-run a specific model with some new values of `y0` / `tEnd`, which have not been covered in the relevant `ClmTask`.
