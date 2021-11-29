import argparse
import json
import plotting_utils as utils


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('datafiles', nargs="+")
    parser.add_argument('outfile')


    args = parser.parse_args()

    collected_data = None

    json_obj = []

    # read input
    for filepath in args.datafiles:
        json_obj += utils.read_json_from_file(filepath)
    
    for entry in json_obj:
        if "methodName" not in entry:
            entry["methodName"] = "ff-soundness"
    
    with open(args.outfile, 'w') as f:
        json.dump(json_obj, f)
        

