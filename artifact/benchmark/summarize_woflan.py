import argparse
import plotting_utils as utils
import numpy
import pandas


def print_statistics(entries):
    print("Printing statistics...")
    print("----------------------------------------")

    print(f"Total entries: {len(entries)}")
    print("----------------------------------------")

    timeout_entries = [entry["sampleName"] for entry in entries if "error" in entry and entry["error"] == "timeout"]
    timeout_entries.sort()

    entries = [
        entry for entry in entries if "error" not in entry or not entry["error"] == "timeout"]
    print(f"Non-timeout-runs: {len(entries)}")
    print("----------------------------------------")

    entries = [
        entry for entry in entries if "error" not in entry or not entry["error"] == "memout"]
    print(f"Non-memout-runs: {len(entries)}")
    print("----------------------------------------")

    woflan_entries = [
        entry for entry in entries if "methodName" in entry and entry["methodName"] == "Woflan"]
    woflan_times = pandas.Series(
        [float(entry["wallTime"])/1000 for entry in woflan_entries])
    print(f"Woflan runs: ")
    print(woflan_times.describe())
    print("----------------------------------------")

    
    unbounded_entries = pandas.Series(
        [float(entry["wallTime"])/1000 for entry in woflan_entries if "The net is not bounded" in entry["diagnosisResult"]])
    print(f"Unbounded entries:")
    print(unbounded_entries.describe())
    print("----------------------------------------")

    dead_entries = pandas.Series(
        [float(entry["wallTime"])/1000 for entry in woflan_entries if "The net contains dead transitions" in entry["diagnosisResult"]])
    print(f"Dead entries:")
    print(dead_entries.describe())
    print("----------------------------------------")


    nonlive_entries = pandas.Series(
        [float(entry["wallTime"])/1000 for entry in woflan_entries if "The net is not live" in entry["diagnosisResult"]])
    print(f"Nonlive entries:")
    print(nonlive_entries.describe())
    print("----------------------------------------")


    for entry in woflan_entries:
        entry.pop("lola_special_commentary", None)
        entry.pop("path", None)
        entry.pop("methodName", None)
        entry.pop("expandedNodes", None)
        entry.pop("lola_stderr", None)
        entry.pop("timeForNetParsing", None)
        entry.pop("numberOfTransitionsAfterBackwardPruning", None)
        entry.pop("timesHeuristicCalculated", None)
        entry.pop("timeTakenForwardPruning", None)
        entry.pop("timeTakenBackwardPruning", None)
        entry.pop("timeTakenPruning", None)
        entry.pop("timeInAlgorithm", None)
        entry.pop("timeInHeuristicCalculation", None)
        entry.pop("ExpandedNodes", None)
        entry.pop("timeHeuristicInit", None)
        entry.pop("timeForFormulaParsing", None)


    print("----------------Slowest 10 entries-------------------------")
    woflan_entries.sort(key=lambda entry: float(entry["wallTime"]))
    print("\n".join(str(entry["sampleName"]) + "; " + str(float(entry["wallTime"])/1000) for entry in woflan_entries[-10:]))

    print("----------------Timeout entries-------------------------")
    print("\n".join(entry for entry in timeout_entries))


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('datafiles', nargs="+")

    args = parser.parse_args()

    collected_data = None

    json_obj = []

    # read input
    for filepath in args.datafiles:
        json_obj += utils.read_json_from_file(filepath)

    print_statistics(json_obj)
