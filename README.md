<div style="display: flex; align-items: center; justify-content: center;">
  <h1>FastForward for Terminating Workflow Nets&nbsp;</h1>
</div>

FastForward is originally a tool for efficiently (semi-)deciding the reachability and coverability problems in Petri nets.
It has since also been expanded with methods for attacking
soundness in workflow nets, particularly structural soundness and generalized soundness, as well as soundness of free-choice workflow nets.

This artifact demonstrates an extension of FastForward with techniques for
several problems related to termination and terminating workflow nets.

*NOTE: The artifact relies on the MILP solver Gurobi.
The solver is proprietary software, and we are not able
to include it in this artifact.
However, with a university affiliation, it is possible to obtain an
academic license. We explain how to install Gurobi in
the section on how to build FastForward.


This README is structured as follows:

- ["Building FastForward"](#compiling-fastforward) explains
how to install FastForward and its dependencies.
This information is helpful for using FastForward outside of this virtual machine. The section is
optional, as this virtual machine comes with a pre-built version of FastForward.
- ["Reproducing experimental results"](#reproducing-experimental-results)
explains how to reproduce the experiments of the paper.
- ["Summarizing experimental results"](#summarizing-experimental-results) explains how to
display the experimental results in a similar manner to that of the paper, to
allow an easy comparison.

For many of the forthcoming steps, we offer two scripts,
one for partial and one for full reproduction.
Use of partial or full reproduction should be consistent,
that is, either the full or partial scripts should be used
for all steps.
By default, we assume reviewers to use partial reproduction,
since the partial evaluation provides the same trends as the full one
while taking much less time.
For full reproduction, reviewers need to regenerate instances - see the corresponding optional section
later in this readme.

## Building FastForward

Make sure you have the following installed on your machine:

* <a href="https://github.com/Z3Prover/z3">Z3 with dotnet bindings</a> (tested under Z3 version 4.8.7)
* <a href="https://theo.informatik.uni-rostock.de/theo-forschung/tools/lola/">Gurobi</a>

Run `sudo sh install.sh` in the root directory.
You may be asked whether to continue installation several times.

Next, navigate to the `artifact/src` folder and
run `./republish.sh` to compile FastForward. 

The expected output looks something like

```
Microsoft (R) Build Engine version 16.7.0+7fb82e5b2 for .NET
Copyright (C) Microsoft Corporation. All rights reserved.

  Determining projects to restore...
  Restored /home/user/fastforward/artifact/src/fastforward.csproj (in 612 ms).
  fastforward -> /home/user/fastforwardartifact/src/bin/Release/netcoreapp3.1/linux-x64/fastforward.dll
  fastforward -> /home/user/fastforwardartifact/src/bin/Release/netcoreapp3.1/linux-x64/publish/
```

## Optional: Regenerating instances

To regenerate benchmark instances, navigate to the
`artifact/benchmark/scripts` folder.
There are two relevant scripts for this step:
* `generate_instances_full.sh` (~24 hours) generates the full set of instances as used in the paper.
* `generate_instances_partial.sh` (~8 hours) generates a partial set of benchmark instances. Running the experiments on this partial set is much faster as there are fewer data points, but the same trends should be visible in the results.

## Reproducing experimental results

Navigate to the
`artifact/benchmark/scripts` folder.
Two scripts are relevant:
* `benchmark_partial.sh` (~1 hour) reproduces the results of the paper on a subset of the benchmark instances, as explained
in ["Partial Reproduction"](#partial-reproduction)
* `benchmark_full.sh` (~6 hours) to reproduce the full set of benchmarks.

If you want more fine-grained control over what is benchmarked,
take a look at the scripts. They call utilities in the `artifact/benchmark/scripts/benchmarking` folder, and those scripts
can also be invoked individually by an interested reviewer.

## Plotting experimental results

Navigate to the
`artifact/benchmark/scripts` folder.
The relevant scripts are
* `plot_results_partial.sh` (~5 min) and
* `plot_results_full.sh` (~5 min)

Execute the script that matches your choice from the previous step.

Afterwards navigate to the 
`artifact/benchmark/plots`
folder and execute

```
latexmk main.tex -pdf
```

The last lines of the output should look like this:
```
Output written on main.pdf (2 pages, 137221 bytes).
Transcript written on main.log.
Latexmk: Log file says output to 'main.pdf'
Latexmk: All targets (main.pdf) are up-to-date
```

Now, to view the resulting pdf execute 
```
evince main.pdf
```

You should see a document with plots similar to
the ones in the paper.
Note that the figure numbering is different than in the paper
due to technical limitations.


Notable folders in this artifact are listed in the following, though this is not a comprehensive table:
- `artifact`: The main folder of the artifact.
  - `benchmark`: All files relating to benchmarking the artifact.
    - `scripts`: Scripts to facilitate benchmarking. Typically, reviewers will benchmark via the scripts here.
    - `instances`: The various benchmark suites mentioned in Section 8 of the paper.
    - `results`: Raw benchmarking results, stored as jsons and used for plotting. If reporting problems, please include this folder.
    - `plots`: Contains the Latex source files to display results as a pdf. Populated from the `results` folder.
    - `tools`: The various tools used in the evaluation (except for LoLA, which is assumed to be installed globally)
  - `src`: The source code. Notable files are:
    - `Soundness/Soundness.cs`: Method `VerifyContinuousSoundness` implements continuous soundness. 
    - `UtilityEntrypoints.cs`: Method `CalculateHeuristicDistance` implements the computation of a lower bound for structural reachability, explained in Section 5 of the paper.
    - `Heuristics/Z3Heuristics.cs` and `Heuristics/Z3Utils.cs` call Z3 to solve various relaxations, e.g. continuous soundness and integer unboundedness
