import argparse
import os

import plotting_utils as utils
import json

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('datafiles', nargs="+")

    args = parser.parse_args()

    collected_data = None

    json_obj = []

    data = []
    headers = ["N", "continuous", "lola", "woflan"]

    size_data = []

    for filepath in args.datafiles:
        # assumes files are named chained_workflows_NUMBER_.json
        entries = utils.read_json_from_file(filepath)

        entries = [entry for entry in entries if entry["methodName"] != "woflan"]
        print(entries)
        with open(filepath, 'w') as f:
                f.write(json.dumps(entries, indent=4))
