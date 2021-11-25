#!/usr/bin/env python3
import os
import glob
from subprocess import check_output, Popen, PIPE, CalledProcessError, TimeoutExpired
import csv
import time
import json
import sys
import benchmark_utils
import pandas
from typing import Dict
import argparse
from typing import List
import tool_handler
import random


def generate_handlers(mode, tool_list):

    result = []
    if "LoLA" in tool_list:
        result.append(tool_handler.LolaHandler())

    if "Woflan" in tool_list:
        result.append(tool_handler.WoflanHandler())

    if "FF-soundness" in tool_list:
        result.append(tool_handler.FastForwardHandler("continuous-sound"))

    if "ICover" in tool_list:
        result.append(tool_handler.ICoverHandler())

    if "Bfc" in tool_list:
        result.append(tool_handler.BFCHandler())

    if "MIST" in tool_list:
        result.append(tool_handler.MISTHandler())

    if "FF-astar-euclidean" in tool_list:
        result.append(tool_handler.FastForwardHandler(
                            "a-star", "-h euclidean", True))
    if "FF-astar-euclidean-nopruning" in tool_list:
        result.append(tool_handler.FastForwardHandler(
                            "a-star", "-h euclidean", False))

    if "FF-GBFS-euclidean" in tool_list:
        result.append(tool_handler.FastForwardHandler(
                            "best-first", "-h euclidean", True))
    if "FF-GBFS-euclidean-nopruning" in tool_list:
        result.append(tool_handler.FastForwardHandler(
                            "best-first", "-h euclidean", False))

    if "FF-astar-qmarkingeq" in tool_list:
        result.append(tool_handler.FastForwardHandler(
                            "a-star", "-h QMarkingEQGurobi", True))
    if "FF-astar-qmarkingeq-nopruning" in tool_list:
        result.append(tool_handler.FastForwardHandler(
                            "a-star", "-h QMarkingEQGurobi", False))

    if "FF-GBFS-qmarkingeq" in tool_list:
        result.append(tool_handler.FastForwardHandler(
                            "best-first", "-h QMarkingEQGurobi", True))
    if "FF-GBFS-qmarkingeq-nopruning" in tool_list:
        result.append(tool_handler.FastForwardHandler(
                            "best-first", "-h QMarkingEQGurobi", False))

    if "FF-Dijkstra" in tool_list:
        result.append(tool_handler.FastForwardHandler(
                            "a-star", "-h zero", True))

    if "FF-Dijkstra-nopruning" in tool_list:
        result.append(tool_handler.FastForwardHandler(
                            "a-star", "-h zero", False))

    if "FF-GBFS-nmarkingeq" in tool_list:
        result.append(tool_handler.FastForwardHandler(
                            "best-first", "-h NMarkingEQGurobi", True))

    if "FF-astar-nmarkingeq" in tool_list:
        result.append(tool_handler.FastForwardHandler(
                            "a-star", "-h NMarkingEQGurobi", True))

    if "FF-astar-qreachability" in tool_list:
        result.append(tool_handler.FastForwardHandler(
                            "a-star", "-h qReachability", True))

    if "FF-saturation-search" in tool_list:
        result.append(tool_handler.FastForwardHandler(
                            "saturation-search", "", True))
    

    if "FF-GBFS-qreachability" in tool_list:
        result.append(tool_handler.FastForwardHandler(
                            "best-first", "-h qReachability", False))
    
    if "FF-GBFS-nmarkingeq-nopruning" in tool_list:
        result.append(tool_handler.FastForwardHandler(
                            "best-first", "-h NMarkingEQGurobi", False))

    if "FF-astar-nmarkingeq-nopruning" in tool_list:
        result.append(tool_handler.FastForwardHandler(
                            "a-star", "-h NMarkingEQGurobi", False))

    if "FF-astar-qreachability-nopruning" in tool_list:
        result.append(tool_handler.FastForwardHandler(
                            "a-star", "-h qReachability", False))

    if "FF-GBFS-qreachability-nopruning" in tool_list:
        result.append(tool_handler.FastForwardHandler(
                            "best-first", "-h qReachability", True))

    if "FF-GBFS-nmarkingeq-nopruning" in tool_list:
        result.append(tool_handler.FastForwardHandler(
                            "best-first", "-h NMarkingEQGurobi", False))
    if "Kosaraju" in tool_list:
        result.append(tool_handler.KosarajuHandler(mode == "cover"))

    return result


def find_propfile(filename_without_ending):
    propfile = filename_without_ending + ".prop"
    if not os.path.isfile(propfile):
        print(f"File {propfile} was not found...")
        propfile = filename_without_ending + ".tts.prop"
        print("Trying " + propfile)
    if not os.path.isfile(propfile):
        print(f"File {propfile} was not found...")
        propfile = filename_without_ending + ".spec.tts.prop"
        print("Trying " + propfile)
    if not os.path.isfile(propfile):
        propfile = None
    else:
        print("Success, running with propfile " + propfile)

    return propfile


def find_netfile(full_filename: str, tool_handler: tool_handler.AbstractToolHandler):
    path = find_file_with_extensions(
        full_filename, tool_handler.get_net_extensions())
    return path


def find_targetfile(full_filename: str, tool_handler: tool_handler.AbstractToolHandler):
    path = find_file_with_extensions(
        full_filename, tool_handler.get_target_extensions())
    return path


def find_files(full_filename: str, tool_handler: tool_handler.AbstractToolHandler):
    return (find_netfile(full_filename, tool_handler), find_targetfile(full_filename, tool_handler))


def find_file_with_extensions(full_filename, extensions):
    for extension in extensions:
        path = full_filename + extension

        if os.path.isfile(path):
            return path
    return None


def run_on_benchmark_suite(tool_handlers, benchmark_suite, output_file, rand, benchmark_prob, first=False, timeout=60):
    benchmark_suite += "/" if not benchmark_suite.endswith("/") else ""

    files = os.listdir(benchmark_suite)
    filenames_without_extensions = set([os.path.splitext(x)[0] for x in files])
    for filename in filenames_without_extensions:
        full_filename = benchmark_suite + filename

        print("--------------------------")
        print("---- " + filename + " ----")
        print("--------------------------")

        random_number = rand.randint(1, 100)
        if random_number > benchmark_prob:
            print(f"Skipping because random number was {random_number}, but probability is {benchmark_prob}")
            continue

        for handler in tool_handlers:
            net_file, target_file = find_files(full_filename, handler)

            tool_name = handler.get_tool_name()

            if net_file is None:
                print(
                    f"Could not find net file for tool {tool_name} and filename {full_filename}")
                continue
            
            if target_file is None:
                print(
                    f"Could not find target file for tool {tool_name} and filename {full_filename}")
                print("Trying to run without target file.")
                target_file = ""
            
            print(
                f"Running {tool_name} on net file {net_file}, target file {target_file}")
            result = handler.run(net_file,
                                    target_file,
                                    timeout)
            result["sampleName"] = filename
            result["netFile"] = net_file
            result["targetFile"] = target_file
            result["methodName"] = handler.get_tool_name()

            output_file.write(
                ("," if not first else "") + json.dumps(result))
            output_file.write("\n")
            output_file.flush()
            first = False
    return first


def benchmark(handlers: List[tool_handler.AbstractToolHandler], benchmark_suites: List[str], output_filepath: str, timeout: int, rand: random.Random, benchmark_probability: int):
    with open(output_filepath, 'w+') as output_file:

        output_file.write("[")
        output_file.flush()
        first = True
        for benchmark_suite in benchmark_suites:
            benchmark_suite += "/" if not benchmark_suite.endswith("/") else ""
            first = run_on_benchmark_suite(
                handlers, benchmark_suite, output_file, rand, benchmark_probability, first, timeout
            )

        output_file.write("]")
        output_file.flush()


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('benchmark_suites', nargs="+",
                        help="One or more directories that contain the input files.")
    parser.add_argument("-o", "--outputfile", type=str, required=True)

    parser.add_argument("-mode", choices=["cover", "reach"], required=True)

    tool_options = [
        "LoLA",
        "Woflan",
        "Bfc",
        "ICover",
        "MIST",
        "FF-saturation-search",
        "FF-astar-qmarkingeq",
        "FF-GBFS-qmarkingeq",
        "FF-astar-qmarkingeq-nopruning",
        "FF-GBFS-qmarkingeq-nopruning",
        "FF-astar-nmarkingeq",
        "FF-GBFS-nmarkingeq",
        "FF-astar-qreachability",
        "FF-GBFS-qreachability",
        "FF-astar-euclidean",
        "FF-astar-euclidean-nopruning",
        "FF-GBFS-euclidean",
        "FF-GBFS-euclidean-nopruning",
        "FF-astar-nmarkingeq-nopruning",
        "FF-GBFS-nmarkingeq-nopruning",
        "FF-astar-qreachability-nopruning",
        "FF-GBFS-qreachability-nopruning",
        "FF-Dijkstra",
        "FF-Dijkstra-nopruning",
        "Kosaraju"
    ]

    parser.add_argument("-t", "--tools", choices=tool_options, nargs="+", required=True, help="A space separated list of tools that should be run")

    parser.add_argument("-to", "--timeout", type=int, required=True, help="The timeout in seconds.")

    parser.add_argument("-p", "--prob", metavar="[0-100]", type=int, required=False, default=100, help="Specified the percentage of instances to actually run on. For example, if --prob 50 is specified, each instance will be run with a probability of 50 percent.")

    parser.add_argument("-s", "--seed", type=int, required=False, default=1, help="Specify the seed used for the random choice of what instances to run when --prob is specified.")

    args = parser.parse_args()

    for tool in args.tools:
        if tool not in tool_options:
            print(f"Tool {tool} is not know, possible options are: {' '.join(tool_options)}")
            exit(25)

    outfile = args.outputfile

    mode = args.mode

    handlers = generate_handlers(mode, args.tools)

    rand = random.Random(args.seed)

    benchmark(handlers, args.benchmark_suites, outfile, args.timeout, rand, args.prob)

    print("Done with main file")
