import argparse
import plotting_utils as utils
import pandas

def find_matching_entry(entry, entries):
    matches = [other for other in entries if other["sampleName"]
               == entry["sampleName"]]
    if len(matches) > 1:
        raise Exception(
            f"Multiple matches for entry {entry['sampleName']} found!")
    if len(matches) == 0:
        return None

    return matches[0]

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('-ho', '--hostfiles', nargs="+", required=True)
    parser.add_argument('-g', '--guestfiles', nargs="+", required=True)

    args = parser.parse_args()

    hostentries = []

    # read input
    for filepath in args.hostfiles:
        hostentries += utils.read_json_from_file(filepath)

    guestentries = []
    for filepath in args.guestfiles:
        guestentries += utils.read_json_from_file(filepath)

    merged_entries = []
    for entry in hostentries:
        guestentry = find_matching_entry(entry, guestentries)
        merged_entries += [(entry, guestentry)]

    for entry in merged_entries:
        if entry[0].get("smallBoundProperties", None) is None or entry[1].get("smallBoundProperties", None) is None:
            continue
        if entry[0]["smallBoundProperties"] == "timeout" and entry[1]["smallBoundProperties"] != "timeout":
            print(f"Host timeout, but guest no timeout: {entry[0]['sampleName']}") 
        if entry[0]["smallBoundProperties"] != "timeout" and entry[1]["smallBoundProperties"] == "timeout":
            print(f"Host no timeout, but guest timeout: {entry[0]['sampleName']}") 

        if entry[0]["smallBoundProperties"].get("minTime", "") != entry[1]["smallBoundProperties"].get("minTime", ""):
            print(f"Host {entry[0]['smallBoundProperties'].get('minTime', '')}, guest {entry[1]['smallBoundProperties'].get('minTime', '')}: {entry[0]['sampleName']})")
