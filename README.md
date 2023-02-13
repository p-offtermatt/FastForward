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



## Building FastForward

FastForward depends on multiple solvers. One is 
Z3 with dotnet bindings, which can be obtained from 
<a href="https://github.com/Z3Prover/z3">https://github.com/Z3Prover/z3</a>.
We tested with version 4.8.7. This version is already included in the virtula machine.

The other required solver is <a href="https://www.gurobi.com/">Gurobi</a>.
Due to licensing issues, we cannot include it in the artifact.
You can obtain a free academic license for Gurobi
on the official website.
Once that is done, please go to
https://www.gurobi.com/downloads/gurobi-software/
and download the Gurobi Optimizer in version v9.0.X (we tested with version 9.0.3).
Note that newer versions will *not* work.
Once you have downloaded the Gurobi Optimizer, follow 
the instructions in Gurobis quickstart guide to install it.
Once this is done, nagivate to the `lib`
folder of the Gurobi Optimizer download, and copy the file `gurobi90.netstandard20.dll`
to this artifact, into the two folders
`src/gurobi` and `tests/gurobi`.


Once this is done, run `sudo sh install.sh` in the root directory.
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

You can test that the installation worked by navigating to the `benchmark/scrips`
folder and executing the script `quicktest.sh`.
This takes about 5 minutes to run, and can give a quick indication
that FastForward is running correctly.

The output of the script should end with the following:

```
Done with main file
Reading input from results/quicktest/test.json
Printing statistics...
10 entries.
===============================
4 timeouts on instances:
A.s00000031__s00001177.lola
A.s00000029__s00001170.lola
A.s00000031__s00001218.lola
A.s00000029__s00001158.lola
===============================
6 error free instances:
A.s00000031__s00001361.lola
A.s00000035__s00001374.lola
A.s00000033__s00001232.lola
A.s00000029__s00001135.lola
A.s00000035__s00001370.lola
A.s00000029__s00001148.lola
===============================
5 entries are not continuously sound
===============================
5 entries are terminating
===============================
Values for the linear constant a_n:
A.s00000031__s00001361.lola has a_n = 12.0
A.s00000035__s00001374.lola has a_n = 31.0
A.s00000033__s00001232.lola has a_n = 9.0
A.s00000029__s00001135.lola has a_n = -1.0
A.s00000035__s00001370.lola has a_n = 11.0
A.s00000029__s00001148.lola has a_n = 25.0
```

If you don't see this output, you can check the results by
taking a look at the file `artifact/benchmark/results/quicktest/test.json`.
If you encountered any errors, you should be able to find them in the log.

## Reproducing experimental results

Navigate to the
`artifact/benchmark/scripts` folder.
The relevant script is
`benchmark.sh`.
It takes about ?? hours to run.
While running, the output should look e.g. like this:
```
---- B2.s00000307__s00004790.lola ----
Computing statistics for net B2.s00000307__s00004790.lola
dotnet fastforward/fastforward.dll statistics nets/workflows/workflows_transformed/IBM/B2.s00000307__s00004790.lola nets/workflows/workflows_transformed/IBM/B2.s00000307__s00004790.formula --cont-sound
```

The script should not output errors for every instance,
but geeting errors very rarely is fine
(e.g. some timeouts and memouts are expected).

If this script takes too long to run for your time budget,
you can skip running continuous soundness on instances,
which takes a large fraction of the total time.
This can be achieved by running the script `benchmark_short.sh`.
The last column of the top table of Fig. 4 will
not be populated if it is done like this.

## Summarizing experimental results

Navigate to the
`artifact/benchmark/scripts` folder.
The relevant script is 
`summarize_benchmarks.sh`.

Executing it outputs the tables from the paper, together with some auxiliary information.
You might want to add this output to a file to view it in a
text editor, for example by running
`summarize_benchmarks.sh | tee log.txt; gedit log.txt`.

## Structure
Notable folders in this artifact are listed in the following, though this is not a comprehensive table:
- `artifact`: The main folder of the artifact.
  - `benchmark`: All files relating to benchmarking the artifact.
    - `scripts`: Scripts to facilitate benchmarking. Typically, reviewers will benchmark via the scripts here.
    - `nets`: The various benchmark instances. Note that `_transformed` refers to
    the original, unreduced nets (transformed to be workflow nets), while `_reduced` refers to
    the workflow nets after reduction rules were applied. 
    - `results`: Raw benchmarking results, stored as json files. If reporting problems, please include this folder.
    - `fastforward`: The compiled binaries for FastForward.
  - `src`: The source code. A notable file is `Heuristics/GurobiHeuristics.cs`, which contains calls to Gurobi. Of particular note are the methods 
      - `CheckIntegerDeadlock`,
      - `CheckForNonnegativeCycle` (which is used to decide termination), 
      - `Compute_A_n`
      - `Unroll_ComputeMinTimeWithBound` 
      - `Unroll_ComputeMaxTimeWithBound`
      - `Unroll_CheckSoundness`

## Troubleshooting

### **FastForward complains about Gurobi, particularly libgurobi90.so**

Check whether Gurobi is installed properly by running
`gurobi_cl`.

### **Running FastForward outputs "Failed to create CoreCLR"**

The first step when encountering this error is to restart the machine. If the error persists, it can mean that dotnet does not have enough memory available. It might be worth trying to
increase the memory size of the virtual machine.

### **Something else went wrong**

If you want to report an error during the review,
please include, e.g. by uploading to Github under an anonymous account, the contents of the folder `artifact/benchmark/results`.