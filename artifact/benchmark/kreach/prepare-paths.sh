#!/bin/bash

# Include current directory in PATH
export PATH=:.:$PATH

# Include cvc4 directory in PATH
export PATH=./cvc4:$PATH

# Make binaries executable
chmod +x kosaraju
chmod +x cvc4/cvc4