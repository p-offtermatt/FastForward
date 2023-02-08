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

def find_matching_entry(entry, entries):
    matches = [other for other in entries if other["sampleName"]
               == entry["sampleName"]]
    if len(matches) > 1:
        raise Exception(
            f"Multiple matches for entry {entry['sampleName']} found!")
    if len(matches) == 0:
        return None

    return matches[0]

def print_statistics(entries, check_entries):
    print("Printing statistics...")
    print(f"Total entries: {len(entries)}")
    print(f"Memouts: ")
    memouts = [
        entry for entry in entries if "error" in entry and "memory" in entry["error"].lower()]
    for entry in memouts:
        print(f"Sample: {entry['sampleName']}")
    print(len(memouts))

    print(f"Timeouts: ")
    timeouts = [
        entry for entry in entries if "error" in entry and "timeout" in entry["error"].lower()]
    print(len(timeouts))


    print("Workflow nets:")
    entries = [
        entry for entry in entries if "isWorkflowNet" in entry and entry["isWorkflowNet"]]
    print(len(entries))
    
    print("Integer unbounded")
    integerUnbounded = [entry for entry in entries if entry["wfIntegerBoundednessCounterexample"] != "None"]
    print(len(integerUnbounded))
    integerBounded = [entry for entry in entries if entry["wfIntegerBoundednessCounterexample"] == "None"]

    print("Continuous unsound")
    contiUnsound = [entry for entry in integerBounded if entry["continuousSoundnessCounterexample"] not in {"None", None}]
    print(len(contiUnsound))
    contiSound = [entry for entry in integerBounded if entry["continuousSoundnessCounterexample"] in {"None", None}]
    
    print("Continuous sound")
    print(len(contiSound))

    print("========= Sanity Checks ==========")

    print("\nProblems: Conti Unsound but no integer deadlock")
    for entry in contiUnsound:
        checkEntry = find_matching_entry(entry, check_entries)
        if not checkEntry["hasIntegerDeadlock"]:
            print(checkEntry["sampleName"])

    print("\nProblems: Integer deadlock but conti sound")
    for entry in check_entries:
        originalEntry = find_matching_entry(entry, entries)
        if entry["hasIntegerDeadlock"] and originalEntry["wfIntegerBoundednessCounterexample"] in {"None", None} and originalEntry["continuousSoundnessCounterexample"] in {"None", None}:
            print(entry["sampleName"])

    print("\nStatistics on time for continuous soundness")
    df = pandas.Series([entry["timeForWFIntegerBoundednessCounterexample"] + entry["timeForContinuousSoundness"] for entry in entries])
    print(df.describe())

    print("\nStatistics on how often continuous soundness is slower")
    
    countContiFaster = 0
    countDeadlocksFaster = 0
    for entry in entries:
        checkEntry = find_matching_entry(entry, check_entries)
        contiSoundTime = entry["timeForWFIntegerBoundednessCounterexample"] + entry["timeForContinuousSoundness"]

        intDeadlockTime = checkEntry["timeForIntegerDeadlock"]
        if contiSoundTime < intDeadlockTime:
            countContiFaster += 1
        if contiSoundTime > 1*intDeadlockTime:
            countDeadlocksFaster += 1

    intDeadlockDf = pandas.Series([entry["timeForIntegerDeadlock"] for entry in check_entries])
    print(f"mean for continuous soundness: {df.mean()} vs {intDeadlockDf.mean()} :mean for integer deadlocks")
    print(f"Integer Deadlocks faster: {countDeadlocksFaster}")
    print(f"Continuous Soundness faster: {countContiFaster}")
    


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('-i', '--inputfiles', nargs="+", required=True)
    parser.add_argument('-c', '--checkAgainst', nargs="+", required=True)

    args = parser.parse_args()

    original_entries = []
    checkAgainst = []

    # read input
    for filepath in args.inputfiles:
        original_entries += utils.read_json_from_file(filepath)

    for filepath in args.checkAgainst:
        checkAgainst += utils.read_json_from_file(filepath)

    print_statistics(original_entries, checkAgainst)
