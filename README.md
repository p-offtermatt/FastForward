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

## Building FastForward

Make sure you have the following installed on your machine:

* <a href="https://dotnet.microsoft.com/">dotnet core</a> with version at least 3.1.403
* <a href="https://github.com/Z3Prover/z3">Z3</a> (tested under Z3 version 4.8.7)

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

## Reproducing experimental results

## Plotting experimental results


# Troubleshooting

### **Running FastForward outputs libgurobi90.so is not installed**
If you see an error message complaining about the lack of `libgurobi90.so`,
it can mean Gurobi is not installed properly, but you compiled with Gurobi.
Check whether you can run the `gurobi_cl` command, or compile without Gurobi by using the script `republish.sh` to compile FastForward.

### **Running FastForward outputs "Failed to create CoreCLR"**

The first step when encountering this error is to restart the machine.
If the error persists, it can mean that dotnet does not have enough memory available.

### **Running with Z3 as a heuristic outputs "Unhandled exception. Microsoft.Z3.Z3Exception: Overflow encountered when expanding old_vector"**

Some instances are too large for Z3 to solve.
You can try running with a different heuristic not using Z3.

# Links

<a href="https://link.springer.com/chapter/10.1007%2F978-3-030-72013-1_1">Blondin M., Haase C., Offtermatt P. (2021) Directed Reachability for Infinite-State Systems. In: Groote J.F., Larsen K.G. (eds) Tools and Algorithms for the Construction and Analysis of Systems. TACAS 2021. Lecture Notes in Computer Science, vol 12652. Springer, Cham. https://doi.org/10.1007/978-3-030-72013-1_1</a>

<a href="https://figshare.com/articles/software/FastForward_A_tool_for_reachability_in_Petri_nets_with_infinite_state_spaces_Artifact_for_the_TACAS21_Contribution_Directed_Reachability_for_Infinite-State_Systems_/13573592">Blondin, Michael; Haase, Christoph; Offtermatt, Philip (2021): FastForward: A tool for reachability in Petri nets with infinite state spaces. Artifact for the TACAS21 Contribution "Directed Reachability for Infinite-State Systems". figshare. Software. https://doi.org/10.6084/m9.figshare.13573592.v1 </a>
