<div style="display: flex; align-items: center; justify-content: center;">
  <h1>FastForward&nbsp;</h1>
</div>


This is the FastForward tool,
a verification tool for Petri nets.

## Installation

First, switch to the source folder:
```
cd artifact/src
```

To compile FastForward <i>without</i> Gurobi enabled, run the following commands:
```
./republish.sh
```

If you want to enable Gurobi bindings, compile like this instead:

```
./republish_with_gurobi.sh
```

Either way, the output should contain a line like this, which tells you where the binary is located:

```
fastforward -> /home/local/username/fastfowardfolder/artifact/src/bin/Debug/netcoreapp3.1/fastforward.dll
```

Simply run 
```
dotnet /home/local/username/fastfowardfolder/artifact/src/bin/Debug/netcoreapp3.1/fastforward.dll
``` 

to invoke the compiled binary.

# Dependencies

Make sure you have the following installed on your machine:

<a href="https://dotnet.microsoft.com/">dotnet core</a> with version at least 3.1.403

<i> Optional dependencies: </i>
<li>
<i>If you want to run with heuristics using the Gurobi solver: </i><br> The newest version of <a href="https://www.gurobi.com/">Gurobi</a> (an academic license is available for employees at degree-granting institutions)
</li>
<li>
<i>If you want to run with heuristics using Z3:</i><br>
The newest version of <a href="https://github.com/Z3Prover/z3">Z3</a>
</li>

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

# References

<a href="https://figshare.com/articles/software/FastForward_A_tool_for_reachability_in_Petri_nets_with_infinite_state_spaces_Artifact_for_the_TACAS21_Contribution_Directed_Reachability_for_Infinite-State_Systems_/13573592">Blondin M., Haase C., Offtermatt P.: Directed Reachability for Infinite-State Systems, TACAS21</a>

<a href="https://link.springer.com/chapter/10.1007%2F978-3-030-72013-1_1">Blondin M., Haase C., Offtermatt P.: FastForward: A tool for reachability in Petri nets with infinite state spaces. Artifact for the TACAS21 Contribution "Directed Reachability for Infinite-State Systems" </a>