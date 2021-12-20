import argparse

from tabulate import tabulate
import plotting_utils as utils
import matplotlib.pyplot as plt


def print_statistics(entries, name):
    print("Printing statistics...")
    print("----------------------------------------")

    print(f"Total entries: {len(entries)}")
    print("----------------------------------------")

    for entry in entries:
        if "error" in entry:
            entry["wallTime"] = 120000

    reachability_entries = [
        entry for entry in entries if "methodName" in entry and entry["methodName"] == "reachability"]
    print(f"Reachability runs: {len(reachability_entries)}")
    print("----------------------------------------")

    continuous_reach_entries = [entry for entry in entries if "methodName" in entry and entry["methodName"]
                  == "continuous"]
    print(f"Continuous reachability runs: {len(continuous_reach_entries)}")
    print("----------------------------------------")


    grouped_entries = utils.group_by_name(continuous_reach_entries + reachability_entries)
    
    grouped_tuples = [(name, entry) for name, entry in grouped_entries.items()]
    grouped_tuples.sort(key=lambda tup: sum([entry["wallTime"] for entry in tup[1]]))

    
    print("----------------Largest 10 entries-------------------------")
    data = []
    for entry in grouped_tuples:
        sample_name = entry[0]
        row = [sample_name] + utils.get_time_from_entries(entry[1])
        data.append(row)

    headers = ["sampleName"] + [entry["methodName"] for entry in grouped_tuples[0][1]]

    for entry in data:
        entry[0] = entry[0].split("-")[0]

    data.sort(key=lambda entry: int(entry[0]))
    print(tabulate(data, headers))

    continuous_data = [entry[1] for entry in data]
    reachability_data = [entry[2] for entry in data]
    x = [entry[0].split("-")[0] for entry in data]
    plt.plot(x,continuous_data, label="continuous")
    plt.plot(x,reachability_data, label="reachability")
    plt.legend()
    plt.title(name)

    print("CONTINUOUS")
    print(" ".join([f"({x[i]},{continuous_data[i]/1000})" for i in range(0, len(x))]))

    print("REACHABILITY")
    print(" ".join([f"({x[i]},{reachability_data[i]/1000})" for i in range(0, len(x))]))

    plt.show()

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('datafiles', nargs="+")

    args = parser.parse_args()

    collected_data = None

    json_obj = []

    for filepath in args.datafiles:
        json_obj += utils.read_json_from_file(filepath)

    print_statistics(json_obj, name=filepath)
