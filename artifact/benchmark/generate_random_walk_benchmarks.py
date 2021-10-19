import argparse
from subprocess import Popen


def call_generate():
    command = "dotnet fastforward/fastforward.dll"
    process = Popen(command.split(" "), stdout=PIPE,
                    stderr=PIPE, preexec_fn=limit_virtual_memory)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "net_folder", help="The directory that contains the input nets in any format.")
    parser.add_argument(
        "output_folder", help="The directory that the output files should be written to.")
