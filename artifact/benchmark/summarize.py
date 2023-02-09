import argparse
import plotting_utils as utils
import numpy
import pandas
import matplotlib.pyplot
import tabulate

spacer = ("=======================")

def removeEntriesWithErrors(entries):
    result = [entry for entry in entries if "error" not in entry]
    print(f"Results without error: {len(result)}")
    timeouts = [entry for entry in entries if "error" in entry and entry["error"] == "timeout"]
    print(f"Timeouts: {len(timeouts)}")

    return result



def printFig4(smallBoundPropEntries,
              continuousSoundnessEntries,
              continuousDeadlockEntries,
              integerDeadlockEntries):
    places = pandas.Series([entry["places"]
                           for entry in smallBoundPropEntries])
    transitions = pandas.Series([entry["transitions"]
                                for entry in smallBoundPropEntries])

    termination = pandas.Series([entry["timeForFastTerminationCheck"]
                                for entry in transformedSmallBoundPropEntries])
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
    table=zip(*table)


    print(tabulate.tabulate(table,
                            headers=["",
                                     "Places",
                                     "Transitions",
                                     "Termination",
                                     "Continuous\nDeadlock",
                                     "Integer\nDeadlock",
                                     "Continuous\nSoundness"],
                            tablefmt="grid"))


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
    transformedSmallBoundPropEntries = removeEntriesWithErrors(transformedSmallBoundPropEntries)

    print("Continuous Soundness on Unreduced Nets")
    transformedContinuousSoundnessEntries = removeEntriesWithErrors(transformedContinuousSoundnessEntries)

    print("Continuous Deadlocks on Unreduced Nets")
    transformedContinuousDeadlockEntries = removeEntriesWithErrors(transformedContinuousDeadlockEntries)

    print("Integer Deadlocks on Unreduced Nets")
    transformedIntegerDeadlockEntries = removeEntriesWithErrors(transformedIntegerDeadlockEntries)
    
    print("Small Bound Properties on Reduced Nets")
    reducedSmallBoundPropEntries = removeEntriesWithErrors(reducedSmallBoundPropEntries)

    print("Continuous Soundness on Reduced Nets")
    reducedContinuousSoundnessEntries = removeEntriesWithErrors(reducedContinuousSoundnessEntries)

    print("Continuous Deadlocks on Reduced Nets")
    reducedContinuousDeadlockEntries = removeEntriesWithErrors(reducedContinuousDeadlockEntries)

    print("Integer Deadlocks on Reduced Nets")
    reducedIntegerDeadlockEntries = removeEntriesWithErrors(reducedIntegerDeadlockEntries)

    print(spacer)

    print("Figure 4: Unreduced instances")
    printFig4(transformedSmallBoundPropEntries,
              transformedContinuousSoundnessEntries,
              transformedContinuousDeadlockEntries,
              transformedIntegerDeadlockEntries)

    print(spacer)
    
    print("Figure 4: Reduced instances")
    printFig4(reducedSmallBoundPropEntries,
              reducedContinuousSoundnessEntries,
              reducedContinuousDeadlockEntries,
              reducedIntegerDeadlockEntries)


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
        args.reducedSmallBoundPropResults)
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
