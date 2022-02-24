import argparse
import os
import sys
from subprocess import Popen, PIPE, CalledProcessError, TimeoutExpired
from pathlib import Path
import inspect

currentdir = os.path.dirname(os.path.abspath(inspect.getfile(inspect.currentframe())))
parentdir = os.path.dirname(currentdir)
sys.path.insert(0, parentdir) 

from plotting.plotting_utils import read_json_from_file


def get_result_from_entry(entry):
    return entry["lola_special_commentary"]["analysis"]["result"]

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("safety_file", help="The file that contains the results of the safety check by LoLA.")
    args = parser.parse_args()
    
    data = read_json_from_file(args.safety_file)
    
    for entry in data:
        if "error" in entry or not get_result_from_entry(entry):
            try:
                os.remove(entry["netFile"])
            except OSError:
                pass

            try:
                os.remove(entry["targetFile"])
            except OSError:
                pass
