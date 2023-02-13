import argparse
import os
import plotting_utils as utils
import numpy
import pandas
import matplotlib.pyplot
import tabulate

spacer = "======================="

bigspacer = "\n" + spacer + "\n" + spacer + "\n"


def removeEntriesWithErrors(entries):
    result = [entry for entry in entries if "error" not in entry]
    print(f"Results without error: {len(result)}")
    timeouts = [
        entry for entry in entries if "error" in entry and entry["error"] == "timeout"]
    print(f"Timeouts: {len(timeouts)}")

    memouts = [
        entry for entry in entries if "error" in entry and "out of memory" in entry["error"].lower()]
    print(f"Memouts: {len(memouts)}")


    nonWorkflowNets = [entry for entry in result if not entry["isWorkflowNet"]]
    print(f"Non-Workflow nets: {len(nonWorkflowNets)}")

    result = [entry for entry in result if entry["isWorkflowNet"]]

    return result

def fig5MakeBottomPartialTable(buckets_with_entries):
    table = [["B", "count with size(N) \in B", "Mean", "Median", "Max"]]
    for bucket in buckets_with_entries:
        range = bucket[0]
        entries = bucket[1]
        df = pandas.Series(entries)
        table += [[f"[{range[0]}, {range[1]})", len(entries), df.mean(), df.median(), df.max()]]
    
    table = zip(*table)
    print(tabulate.tabulate(table, tablefmt="grid"))

def iterate_over_range(function, entries):
    result = []
    for range in [(0,20), (20,60), (60, 150), (150, 405)]:
        entries_in_range = [
            entry for entry in entries if range[0] <= entry["places"] + entry["transitions"] < range[1]]

        result += [(range, [function(entry) for entry in entries_in_range])]
    return result

def printFig5(smallBoundPropEntries):
    fastTerminatingEntries = [
        entry for entry in smallBoundPropEntries if entry["hasFastTermination"]]
    
    linFractions = [entry["smallBoundProperties"]["A_n"] /
                    entry["transitions"] for entry in fastTerminatingEntries]

    table = [["B", "Count with L \in B"]]

    for range in [(0, 0.75), (0.75, 1), (1, 1.0001), (1.0001, 1.75), (1.75, 1000)]:
        entries_in_range = [
            entry for entry in linFractions if range[0] <= entry < range[1]]
        table += [[f"[{range[0]}, {range[1]})", len(entries_in_range)]]

    print("Top:")
    table = zip(*table)
    print(tabulate.tabulate(table, tablefmt="grid"))

    print(bigspacer)

    print("Middle:")
    timeouts = [entry for entry in fastTerminatingEntries if entry["smallBoundProperties"]
                ["maxTime"] == "timeout" or entry["smallBoundProperties"]["minTime"] == "timeout"]
    print(
        f"Filtering out {len(timeouts)} instances where MinTime(1) or MaxTime(1) timed out")

    minusOnes = [entry for entry in fastTerminatingEntries if entry["smallBoundProperties"]
                 ["maxTime"] == -1 or entry["smallBoundProperties"]["minTime"] == -1]
    print(
        f"Filtering out {len(minusOnes)} instances where MinTime(1) or MaxTime(1) are infinity")
    
    fastTerminatingEntries = [entry for entry in fastTerminatingEntries if entry["smallBoundProperties"]["maxTime"] not in {-1, "timeout"}
                  and entry["smallBoundProperties"]["minTime"] not in {-1, "timeout"}]

    table = [["", "Count with D \in B"]]

    differences = []
    for entry in fastTerminatingEntries:
        maxTime = entry["smallBoundProperties"]["maxTime"]
        normalizedMaxTime = maxTime / int(entry["transitions"]) 
        minTime = entry["smallBoundProperties"]["minTime"]
        normalizedMinTime = minTime / int(entry["transitions"])


        differences.append([normalizedMaxTime-normalizedMinTime, entry])

    for range in [(0, 0.05), (0.05, 0.15), (0.15, 0.3), (0.3, 0.5), (0.5, 1000)]:
        entries_in_range = [
            entry for entry in linFractions if range[0] <= entry < range[1]]
        table.append([f"[{range[0]}, {range[1]})", len(entries_in_range)])

    table = zip(*table)
    print(tabulate.tabulate(table, tablefmt="grid"))

    print(bigspacer)
    print("Bottom:")

    anBuckets = iterate_over_range(lambda entry: entry["smallBoundProperties"]["timeForComputingA_n"], fastTerminatingEntries)
    minTimeBuckets = iterate_over_range(lambda entry: entry["smallBoundProperties"]["timeForComputingMinTime"], fastTerminatingEntries)
    maxTimeBuckets = iterate_over_range(lambda entry: entry["smallBoundProperties"]["timeForComputingMaxTime"], fastTerminatingEntries)

    print("Analysis time for computing a_N (in ms):")
    fig5MakeBottomPartialTable(anBuckets)

    print(bigspacer)
    print("Analysis time for computing MinTime(1) (in ms):")
    fig5MakeBottomPartialTable(minTimeBuckets)

    print(bigspacer)
    print("Analysis time for computing MaxTime(1) (in ms):")
    fig5MakeBottomPartialTable(maxTimeBuckets)

def printFig4(smallBoundPropEntries,
              continuousSoundnessEntries,
              continuousDeadlockEntries,
              integerDeadlockEntries):
    places = pandas.Series([entry["places"]
                           for entry in smallBoundPropEntries])
    transitions = pandas.Series([entry["transitions"]
                                for entry in smallBoundPropEntries])

    termination = pandas.Series([entry["timeForFastTerminationCheck"]
                                for entry in smallBoundPropEntries])
    continuousDeadlock = pandas.Series(
        [entry["timeForContinuousDeadlock"] for entry in continuousDeadlockEntries])
    integerDeadlock = pandas.Series(
        [entry["timeForIntegerDeadlock"] for entry in integerDeadlockEntries])
    continuousSoundness = pandas.Series(
        [entry["timeForContinuousSoundness"] for entry in continuousSoundnessEntries])

    def makeColumn(entries):
        return [entries.mean(), entries.median(), entries.max()]

    table = [["Mean", "Median", "Max"],
             makeColumn(places),
             makeColumn(transitions),
             makeColumn(termination),
             makeColumn(continuousDeadlock),
             makeColumn(integerDeadlock),
             makeColumn(continuousSoundness)
             ]

    # transpose table
    table = zip(*table)

    print("Top:")

    print(tabulate.tabulate(table,
                            headers=["",
                                     "Places",
                                     "Transitions",
                                     "Termination",
                                     "Continuous\nDeadlock",
                                     "Integer\nDeadlock",
                                     "Continuous\nSoundness"],
                            tablefmt="grid"))

    print("Bottom:")
    totalTerminating = [
        entry for entry in integerDeadlockEntries if entry["hasFastTermination"]]
    totalNonterminating = [
        entry for entry in integerDeadlockEntries if not entry["hasFastTermination"]]

    terminatingDeadlocks = [
        entry for entry in totalTerminating if entry["hasIntegerDeadlock"] or entry["hasContinuousDeadlock"]]
    nonterminatingDeadlocks = [
        entry for entry in totalNonterminating if entry["hasIntegerDeadlock"] or entry["hasContinuousDeadlock"]]

    table = [["", "Terminating", "Non-\nterminating"],
             ["Total", len(totalTerminating), len(totalNonterminating)],
             ["Deadlocking\n(Not generalised sound)", len(terminatingDeadlocks), len(nonterminatingDeadlocks)]]

    table = zip(*table)
    print(tabulate.tabulate(table, tablefmt="grid"))


def print_statistics(transformedSmallBoundPropEntries,
                     transformedContinuousSoundnessEntries,
                     transformedContinuousDeadlockEntries,
                     transformedIntegerDeadlockEntries,
                     reducedSmallBoundPropEntries,
                     reducedContinuousSoundnessEntries,
                     reducedContinuousDeadlockEntries,
                     reducedIntegerDeadlockEntries):
    print("Printing summary...")

    print(spacer)
    print("Removing entries with errors (timeouts, memouts, ...)")
    print("Small Bound Properties on Unreduced Nets")
    transformedSmallBoundPropEntries = removeEntriesWithErrors(
        transformedSmallBoundPropEntries)

    print(spacer)
    print("Continuous Soundness on Unreduced Nets")
    transformedContinuousSoundnessEntries = removeEntriesWithErrors(
        transformedContinuousSoundnessEntries)

    print(spacer)
    print("Continuous Deadlocks on Unreduced Nets")
    transformedContinuousDeadlockEntries = removeEntriesWithErrors(
        transformedContinuousDeadlockEntries)

    print(spacer)
    print("Integer Deadlocks on Unreduced Nets")
    transformedIntegerDeadlockEntries = removeEntriesWithErrors(
        transformedIntegerDeadlockEntries)

    print(spacer)
    print("Small Bound Properties on Reduced Nets")
    reducedSmallBoundPropEntries = removeEntriesWithErrors(
        reducedSmallBoundPropEntries)

    print(spacer)
    print("Continuous Soundness on Reduced Nets")
    reducedContinuousSoundnessEntries = removeEntriesWithErrors(
        reducedContinuousSoundnessEntries)

    print(spacer)
    print("Continuous Deadlocks on Reduced Nets")
    reducedContinuousDeadlockEntries = removeEntriesWithErrors(
        reducedContinuousDeadlockEntries)

    print(spacer)
    print("Integer Deadlocks on Reduced Nets")
    reducedIntegerDeadlockEntries = removeEntriesWithErrors(
        reducedIntegerDeadlockEntries)

    print(bigspacer)

    print("Figure 4: Unreduced instances")
    printFig4(transformedSmallBoundPropEntries,
              transformedContinuousSoundnessEntries,
              transformedContinuousDeadlockEntries,
              transformedIntegerDeadlockEntries)

    print(bigspacer)

    print("Figure 4: Reduced instances")
    printFig4(reducedSmallBoundPropEntries,
              reducedContinuousSoundnessEntries,
              reducedContinuousDeadlockEntries,
              reducedIntegerDeadlockEntries)
    
    print(bigspacer)
    
    printFig5(transformedSmallBoundPropEntries)


def find_matching_entry(entry, entries):
    matches = [other for other in entries if other["sampleName"]
               == entry["sampleName"]]
    if len(matches) > 1:
        raise Exception(
            f"Multiple matches for entry {entry['sampleName']} found!")
    if len(matches) == 0:
        return None

    return matches[0]


def read_from_filepaths_into_list(filepaths):
    result = []
    for filepath in filepaths:
        if os.path.isfile(filepath):
            result += utils.read_json_from_file(filepath)
    return result


if __name__ == "__main__":
    parser = argparse.ArgumentParser()

    # parse results for transformed nets
    parser.add_argument(
        '-ts', '--transformedSmallBoundPropResults', nargs="+", required=True)
    parser.add_argument(
        '-tcs', '--transformedContinuousSoundnessResults', nargs="+", required=True)
    parser.add_argument(
        '-tcd', '--transformedContinuousDeadlockResults', nargs="+", required=True)
    parser.add_argument(
        '-tid', '--transformedIntegerDeadlockResults', nargs="+", required=True)

    # parse results for reduced nets
    parser.add_argument(
        '-rs', '--reducedSmallBoundPropResults', nargs="+", required=True)
    parser.add_argument(
        '-rcs', '--reducedContinuousSoundnessResults', nargs="+", required=True)
    parser.add_argument(
        '-rcd', '--reducedContinuousDeadlockResults', nargs="+", required=True)
    parser.add_argument(
        '-rid', '--reducedIntegerDeadlockResults', nargs="+", required=True)

    args = parser.parse_args()

    transformedSmallBoundPropEntries = read_from_filepaths_into_list(
        args.transformedSmallBoundPropResults)
    transformedContinuousSoundnessEntries = read_from_filepaths_into_list(
        args.transformedContinuousSoundnessResults)
    transformedContinuousDeadlockEntries = read_from_filepaths_into_list(
        args.transformedContinuousDeadlockResults)
    transformedIntegerDeadlockEntries = read_from_filepaths_into_list(
        args.transformedIntegerDeadlockResults)

    reducedSmallBoundPropEntries = read_from_filepaths_into_list(
        args.reducedSmallBoundPropResults)
    reducedContinuousSoundnessEntries = read_from_filepaths_into_list(
        args.reducedContinuousSoundnessResults)
    reducedContinuousDeadlockEntries = read_from_filepaths_into_list(
        args.reducedContinuousDeadlockResults)
    reducedIntegerDeadlockEntries = read_from_filepaths_into_list(
        args.reducedIntegerDeadlockResults)

    print_statistics(
        transformedSmallBoundPropEntries,
        transformedContinuousSoundnessEntries,
        transformedContinuousDeadlockEntries,
        transformedIntegerDeadlockEntries,
        reducedSmallBoundPropEntries,
        reducedContinuousSoundnessEntries,
        reducedContinuousDeadlockEntries,
        reducedIntegerDeadlockEntries
    )
