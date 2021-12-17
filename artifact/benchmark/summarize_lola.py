import argparse
import plotting_utils as utils
import pandas


def print_statistics(entries):
    print("Printing statistics...")
    print("----------------------------------------")

    print(f"Total entries: {len(entries)}")
    print("----------------------------------------")

    error_entries = [entry["sampleName"] for entry in entries if "error" in entry and (entry["error"] == "timeout" or entry["error"] == "memout")]
    error_entries.sort()

    entries = [
        entry for entry in entries if "error" not in entry or not entry["error"] == "timeout"]
    print(f"Non-timeout-runs: {len(entries)}")
    print("----------------------------------------")

    entries = [
        entry for entry in entries if "error" not in entry or not entry["error"] == "memout"]
    print(f"Non-memout-runs: {len(entries)}")
    print("----------------------------------------")

    lola_entries = [
        entry for entry in entries if "methodName" in entry and entry["methodName"] == "reachability"]
    print(f"LoLA runs: {len(lola_entries)}")
    print("----------------------------------------")

    sound_entries = pandas.Series(
        [entry["wallTime"]/1000 for entry in lola_entries if entry["lola_special_commentary"]["analysis"]["result"]])
    print(f"Sound entries:")
    print(sound_entries.describe())
    print("----------------------------------------")

    unsound_entries = pandas.Series(
        [entry["wallTime"]/1000 for entry in lola_entries if not entry["lola_special_commentary"]["analysis"]["result"]])
    print(f"Unsound entries:")
    print(unsound_entries.describe())
    print("----------------------------------------")

    # ff_entries = [entry for entry in entries if "methodName" in entry and entry["methodName"]
    #               == "FastForward_a-starQMarkingEQGurobi+Pruning"]
    # print(f"FF runs: {len(ff_entries)}")
    # print("----------------------------------------")

    # sound_entries = pandas.Series(
    #     [entry["wallTime"]/1000 for entry in ff_entries if entry["path"] != "unreachable"])
    # print(f"Sound entries:")
    # print(sound_entries.describe())
    # print("----------------------------------------")


    for entry in lola_entries:
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
    lola_entries.sort(key=lambda entry: entry["wallTime"])
    print("\n".join(str(entry["sampleName"]) + "; " + str(float(entry["wallTime"]) / 1000) for entry in lola_entries[-10:]))

    print("----------------Time/memout entries-------------------------")
    print("\n".join(entry for entry in error_entries))

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
