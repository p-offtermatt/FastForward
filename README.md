<div style="display: flex; align-items: center; justify-content: center;">
  <h1>FastForward for Soundness&nbsp;</h1>
</div>

FastForward is originally a tool for efficiently (semi-)deciding the reachability and coverability problems in Petri nets.
This artifact showcases an extension of FastForward with methods for attacking
soundness in workflow nets, particularly structural soundness and generalized soundness, as well as soundness of free-choice workflow nets.

This README is structured as follows:

- OPTIONAL: ["Building FastForward"](#compiling-fastforward) explains
how to build FastForward and its dependencies.
This information is helpful for using FastForward outside of this virtual machine. The section is
optional, as this virtual machine comes with a pre-built version of FastForward.
- OPTIONAL: ["Regenerating instances"](#regenerating-instances) explains how to
regenerate the benchmark instances.
The scripts used in this section can help understanding how these instances are obtained from the original benchmark sets.
This section is optional, as this virtual machine contains the pre-generated benchmark instances.
- ["Reproducing experimental results"](#reproducing-experimental-results)
explains how to reproduce the experiments of the paper.
- ["Plotting experimental results"](#plotting-experimental-results) explains how to transform the data generated in the previous step
to plots similar to the ones in the paper.

In the interest of time, we offer
scripts to reproduce (in shorter time)
a subset of results.
The plots of the paper can be reproduced with this subset, with the following differences:
* For Fig. 3, 4 nets are used per size, rather than 20 as in the paper.
* For Figs. 5 and 6, instead of having an instance for every number from 1 to 40,
there are instances for every fourth number.
* The timeout for all experiments is 60 seconds, instead of the 120 seconds used in the paper.

For many of the forthcoming steps, we offer two scripts,
one for partial and one for full reproduction.
Use of partial or full reproduction should be consistent,
that is, either the full or partial scripts should be used
for all steps.

## Optional: Building FastForward

Make sure you have the following installed on your machine:

* <a href="https://github.com/Z3Prover/z3">Z3 with dotnet bindings</a> (tested under Z3 version 4.8.7)
* <a href="https://theo.informatik.uni-rostock.de/theo-forschung/tools/lola/">lola2.0</a> See lola's readme for instructions. lola should be on your path, e.g. running `lola` should give output like this:
```
lola: NET
lola:   reading net from stdin
```

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
* `generate_instances_full.sh` generates the full set of instances as used in the paper.
* `generate_instances_partial.sh` generates a partial set of benchmark instances. Running the experiments on this partial set is much faster as there are fewer data points, but the same trends should be visible in the results.

## Reproducing experimental results

Navigate to the
`artifact/benchmark/scripts` folder.
Two scripts are relevant:
* `benchmark_partial.sh` reproduces the results of the paper on a subset of the benchmark instances, as explained
in ["Partial Reproduction"](#partial-reproduction)
* `benchmark_full.sh` to reproduce the full set of benchmarks.

If you want more fine-grained control over what is benchmarked,
take a look at the scripts. They call utilities in the `artifact/benchmark/scripts/benchmarking` folder, and those scripts
can also be invoked individually by an interested reviewer.

## Plotting experimental results

Navigate to the
`artifact/benchmark/scripts` folder.
The relevant scripts are
* `plot_results_partial.sh` and
* `plot_results_full.sh`

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


# Links

TODO: Figshare link
