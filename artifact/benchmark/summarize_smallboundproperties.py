import argparse
import plotting_utils as utils
import pandas

# tuples = [(238, 147),
# (171, 125),
# (201, 133),
# (114, 73),
# (204, 143),
# (216, 154),
# (219, 124),
# (207, 146)]
# print(pandas.Series([tuple[0] for tuple in tuples]).describe())
# print(pandas.Series([tuple[1] for tuple in tuples]).describe())


def remove_usual_file_endings(input: str):
    while True:
        if input.endswith(".xml"):
            input = input[:-(len(".xml"))]
        elif input.endswith(".lola"):
            input = input[:-(len(".lola"))]
        elif input.endswith(".tpn"):
            input = input[:-(len(".tpn"))]
        else:
            return input


def print_statistics(entries):
    print("Printing statistics...")
    print(f"Total entries: {len(entries)}")
    print(f"Timeouts: ")
    timeouts = [
        entry for entry in entries if "error" in entry and "memory" in entry["error"].lower()]
    for entry in timeouts:
        print(f"Sample: {entry['sampleName']}")
    print(len(timeouts))

    print("Workflow nets:")
    entries = [
        entry for entry in entries if "isWorkflowNet" in entry and entry["isWorkflowNet"]]
    print(len(entries))

    print("Number of entries without smallboundproperties:")
    notSmallBoundProps = [
        entry for entry in entries if "smallBoundProperties" not in entry or entry["smallBoundProperties"] is None]
    print(len(notSmallBoundProps))

    smallBoundProps = [
        entry for entry in entries if "smallBoundProperties" in entry and entry["smallBoundProperties"] != None]

    print("\nSanity checks: Fast Termination iff A_n != -1:")
    print("Entries with fast termination but A_n == -1:")
    print([entry["sampleName"] for entry in smallBoundProps if entry["hasFastTermination"]
          and entry["smallBoundProperties"]["A_n"] == -1])
    print("Entries without fast termination but A_n != -1:")
    print([entry["sampleName"] for entry in smallBoundProps if not entry["hasFastTermination"]
          and entry["smallBoundProperties"]["A_n"] != -1])
    print("\n")

    print("Number of fast-termination entries:")
    print(len([entry for entry in smallBoundProps if entry["hasFastTermination"]]))
    print("\n")

    smallBoundProps = [
        entry for entry in smallBoundProps if entry["hasFastTermination"]]

    # print("====== INFO ABOUT A_n ========")
    # PrintAnInfo(smallBoundProps)
    # print("==============================")

    # print("====== INFO ABOUT Min/MaxTime ========")
    # PrintMinMaxTimeInfo(smallBoundProps)
    # print("==============================")


    # print("====== INFO ABOUT Soundness ========")
    # PrintSoundnessInfo(smallBoundProps)
    # print("==============================")

    print("====== INFO ABOUT Computation Time ========")
    PrintNetSizeToComputationTimeInfo(smallBoundProps)
    print("==============================")

def iterate_over_range(function, entries):
    print("Buckets.")
    print("--------------")
    for range in [(0,20), (20,60), (60, 150), (150, 405)]:
        entries_in_range = [
            entry for entry in entries if range[0] <= entry["places"] + entry["transitions"] < range[1]]
        total = len(entries_in_range)

        print(f"Total in Bucket [{range[0]}, {range[1]}): {len(entries_in_range)}")

        function(entries_in_range)

def GetTimeForMean(entries, key):
    df = pandas.Series([key(entry) for entry in entries])
    print(df.mean())

def GetTimeForMedian(entries, key):
    df = pandas.Series([key(entry) for entry in entries])
    print(df.median())

def GetTimeForMax(entries, key):
    df = pandas.Series([key(entry) for entry in entries])
    print(df.max())

def GetKeyStats(entries, key):
    print("Mean.")
    iterate_over_range(lambda entries: GetTimeForMean(entries, key), entries)
    print("----------")
    print("Median.")
    iterate_over_range(lambda entries: GetTimeForMedian(entries, key), entries)
    print("----------")
    print("Max.")
    iterate_over_range(lambda entries: GetTimeForMax(entries, key), entries)
    print("----------")


def PrintNetSizeToComputationTimeInfo(smallBoundProps):
    print("Timeouts")
    minTimeTimeouts = [entry["sampleName"]
                       for entry in smallBoundProps if entry["smallBoundProperties"]["timeForComputingMinTime"] == "timeout" and entry["smallBoundProperties"]["timeForComputingMaxTime"] != "timeout"]
    maxTimeTimeouts = [entry["sampleName"]
                       for entry in smallBoundProps if entry["smallBoundProperties"]["timeForComputingMaxTime"] == "timeout"
                       and
                       entry["smallBoundProperties"]["timeForComputingMinTime"] != "timeout"]

    bothTimeout = [entry["sampleName"]
                   for entry in smallBoundProps if entry["smallBoundProperties"]["timeForComputingMaxTime"] == "timeout"
                   and
                   entry["smallBoundProperties"]["timeForComputingMinTime"] == "timeout"]
    print(len(minTimeTimeouts))
    print(len(maxTimeTimeouts))
    print(len(bothTimeout))

    nonTimeouts = [entry for entry in smallBoundProps if entry["smallBoundProperties"]["timeForComputingMinTime"] != "timeout"
                   and entry["smallBoundProperties"]["timeForComputingMaxTime"] != "timeout"]

    print("Non Timeouts")
    print(len(nonTimeouts))
    print("Buckets with net sizes. Time for A_n, mins, and maxs:")

    print("Times for A_n.")
    GetKeyStats(nonTimeouts, lambda entry: entry["smallBoundProperties"]["timeForComputingA_n"])
    print("Values for A_n.")

    GetKeyStats(nonTimeouts, lambda entry: entry["smallBoundProperties"]["A_n"]/entry["transitions"])

    print("Maximal Time for A_n:")
    print(max([entry["smallBoundProperties"]["timeForComputingA_n"] for entry in nonTimeouts]))

    print("Times for MinTime.")
    GetKeyStats(nonTimeouts, lambda entry: entry["smallBoundProperties"]["timeForComputingMinTime"])

    print("Times for MaxTime.")
    GetKeyStats(nonTimeouts, lambda entry: entry["smallBoundProperties"]["timeForComputingMaxTime"])


def PrintMinMaxTimeInfo(smallBoundProps):

    print("Timeouts")
    minTimeTimeouts = [entry["sampleName"]
                       for entry in smallBoundProps if entry["smallBoundProperties"]["timeForComputingMinTime"] == "timeout" and entry["smallBoundProperties"]["timeForComputingMaxTime"] != "timeout"]
    maxTimeTimeouts = [entry["sampleName"]
                       for entry in smallBoundProps if entry["smallBoundProperties"]["timeForComputingMaxTime"] == "timeout"
                       and
                       entry["smallBoundProperties"]["timeForComputingMinTime"] != "timeout"]

    bothTimeout = [entry["sampleName"]
                   for entry in smallBoundProps if entry["smallBoundProperties"]["timeForComputingMaxTime"] == "timeout"
                   and
                   entry["smallBoundProperties"]["timeForComputingMinTime"] == "timeout"]
    print(len(minTimeTimeouts))
    print(len(maxTimeTimeouts))
    print(len(bothTimeout))

    nonTimeouts = [entry for entry in smallBoundProps if entry["smallBoundProperties"]["timeForComputingMinTime"] != "timeout"
                   and entry["smallBoundProperties"]["timeForComputingMaxTime"] != "timeout"]

    differences = []

    minusOnes = 0
    for entry in nonTimeouts:
        maxTime = entry["smallBoundProperties"]["maxTime"]
        normalizedMaxTime = maxTime / \
            int(entry["transitions"]) if maxTime != -1 else -1
        minTime = entry["smallBoundProperties"]["minTime"]
        normalizedMinTime = minTime / \
            int(entry["transitions"]) if minTime != -1 else -1

        if minTime == -1:
            minusOnes += 1
            continue

        differences.append([normalizedMaxTime-normalizedMinTime, entry])

    print(f"Instances with MinTime=-1: {minusOnes}")

    print("Buckets with differences. Time for mins:")
    for range in [(-1, 0), (0, 0.05), (0.05, 0.15), (0.15, 0.3), (0.3, 0.5), (0.5, 1000)]:
        entries_in_range = [
            entry for entry in differences if range[0] <= entry[0] < range[1]]
        total = len(entries_in_range)
        print(f"[{range[0]}, {range[1]}): {total}")
        # df = pandas.Series([entry[1]["smallBoundProperties"]["timeForComputingMinTime"] for entry in entries_in_range])
        # print(df.describe())
        df = pandas.Series([entry[1]["transitions"] + entry[1]["places"]
                           for entry in entries_in_range])
        print(df.describe())

    # print("Buckets with differences. Time for maxs:")
    # for range in [(-1, 0), (0, 0.05), (0.05, 0.15), (0.15, 0.3), (0.3, 0.5), (0.5, 1000)]:
    #     entries_in_range = [entry for entry in differences if range[0] <= entry[0] < range[1]]
    #     total = len(entries_in_range)
    #     print(f"[{range[0]}, {range[1]}): {total}")
    #     df = pandas.Series([entry[1]["smallBoundProperties"]["timeForComputingMaxTime"] for entry in entries_in_range])
    #     print(df.describe())


def PrintAnInfo(smallBoundProps):
    ans = [(prop["smallBoundProperties"]["A_n"]/prop["transitions"],
            prop["smallBoundProperties"]["timeForComputingA_n"]) for prop in smallBoundProps]
    ans.sort(key=lambda x: x[0])

    for range in [(-1, 0), (0, 0.75), (0.75, 1), (1, 1.00000000000001), (1.00000000000001, 1.75), (1.75, 1000)]:
        entries_in_range = [an for an in ans if range[0] <= an[0] < range[1]]
        total = len(entries_in_range)
        print(f"[{range[0]}, {range[1]}): {total}")
        df = pandas.Series([entry[1] for entry in entries_in_range])
        print(df.describe())


def PrintSoundnessInfo(smallBoundProps):
    timeouts = [entry
                       for entry in smallBoundProps if entry["smallBoundProperties"]["timeForComputingSoundnessViaUnrolling"] == "timeout"]

    nonTimeouts = [entry for entry in smallBoundProps if entry["smallBoundProperties"]["timeForComputingSoundnessViaUnrolling"] != "timeout"]
    ans = [(prop["smallBoundProperties"]["A_n"],
            prop["smallBoundProperties"]["timeForComputingSoundnessViaUnrolling"]) for prop in nonTimeouts]

    for range in [(1, 10), (10, 25), (25, 50), (50, 100), (100, 9999999999)]:
        entries_in_range = [an for an in ans if range[0] <= an[0] < range[1]]
        total = len(entries_in_range)
        print(f"[{range[0]}, {range[1]}): {total}")
        df = pandas.Series([entry[1] for entry in entries_in_range])
        print(df.describe())

    print("=TOTAL=")
    df = pandas.Series([entry[1] for entry in ans])
    print(df.describe())
    print("TIMEOUTS")
    df = pandas.Series([entry["smallBoundProperties"]["A_n"] for entry in timeouts])
    print(df.describe())

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('-i', '--inputfiles', nargs="+", required=True)

    args = parser.parse_args()

    original_entries = []

    # read input
    for filepath in args.inputfiles:
        original_entries += utils.read_json_from_file(filepath)

    print_statistics(original_entries)
