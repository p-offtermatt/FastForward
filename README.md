<div style="display: flex; align-items: center; justify-content: center;">
  <h1>FastForward for Soundness&nbsp;</h1>
</div>

FastForward is originally a tool for efficiently (semi-)deciding the reachability and coverability problems in Petri nets.
This artifact showcases an extension of FastForward with methods for attacking
soundness in workflow nets, particularly structural soundness and generalized soundness, as well as soundness of free-choice workflow nets.

This README is structured as follows:

- ["Reproducing experimental results"](#reproducing-experimental-results)
explains how to reproduce the experiments of the paper.
- ["Plotting experimental results"](#plotting-experimental-results) explains how to transform the data generated in the previous step
to plots similar to the ones in the paper.

- OPTIONAL: ["Building FastForward"](#compiling-fastforward) explains
how to build FastForward and its dependencies.
This information is helpful for using FastForward outside of this virtual machine. The section is
optional, as this virtual machine comes with a pre-built version of FastForward.
- OPTIONAL: ["Regenerating instances"](#regenerating-instances) explains how to
regenerate the benchmark instances.
The scripts used in this section can help understanding how these instances are obtained from the original benchmark sets.
This section is optional, as this virtual machine contains the pre-generated benchmark instances.

## Reproducing experimental results

To reproduce experimental results, navigate to the
`artifact/benchmark/scripts` folder.
Two scripts are relevant:
* By default, we assume reviewers to use `benchmark_partial.sh`.
This reproduces the results of the paper on a subset of the benchmark instances, and with a lower timeout of 60 seconds rather than 120 seconds as in the paper.
* OPTIONAL: `benchmark_full.sh` runs the full set of benchmarks from the paper with the same time limit. Note that the full set of benchmark instances should be generated to use this script; see ["Regenerating instances"](#regenerating-instances).

If you want more fine-grained control over what is benchmarked,
take a look at the scripts. They call utilities in the `artifact/benchmark/scripts/benchmarking` folder, and those scripts
can also be invoked individually by an interested reviewer.

## Plotting experimental results

## Building FastForward

Make sure you have the following installed on your machine:

* <a href="https://dotnet.microsoft.com/">dotnet core</a> with version at least 3.1.403
* <a href="https://github.com/Z3Prover/z3">Z3</a> (tested under Z3 version 4.8.7)
* <a href="https://theo.informatik.uni-rostock.de/theo-forschung/tools/lola/">lola2.0</a> See lola's readme for instructions. lola should be on your path, e.g. running `lola` should give output like this:
```
lola: NET
lola:   reading net from stdin
```
* <a href="https://www.winehq.org/">Wine</a> (can be installed via the apt package manager)

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

## Regenerating instances

To regenerate benchmark instances, navigate to the
`artifact/benchmark/scripts` folder.
There are two relevant scripts for this step:
* `generate_instances_full.sh` generates the full set of instances as used in the paper.
* `generate_instances_partial.sh` generates a partial set of benchmark instances. Running the experiments on this partial set is much faster as there are fewer data points, but the same trends should be visible in the results.


# Links

TODO: Figshare link
