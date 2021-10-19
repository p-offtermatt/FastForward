import argparse
from subprocess import run
import os

lengths = [10, 15, 20, 25, 30, 35, 40, 50, 60, 75, 90, 100]
netsPerLength = 1

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('input_files', nargs="+",
                        help="One or more input nets.")
    parser.add_argument("-o", "--outputpath", type=str, required=True)
    parser.add_argument("-m", "--mode", type=str, required=True, choices=["Reachability", "Coverability"])
    parser.add_argument("-f", "--format", type=str, required=True, choices=["TTS", "Dotspec", "Lola"])
    parser.add_argument("-p", "--prune", action='store_true')




    args = parser.parse_args()
    print(args.prune)

    for input_file in args.input_files:
        basename = os.path.splitext(os.path.basename(input_file))[0]
        for length in lengths:
            for i in range(0, netsPerLength):
                for parameterized in [""]: #["--single", ""]:
                    parameterized_text = "single" if parameterized == "--single" else "multi"
                    output_filename = f"{args.outputpath}/{basename}_{parameterized_text}_{length}_{i}"
                    print("Writing to " + output_filename)
                    command = f"dotnet fastforward/fastforward.dll generate {input_file} {length} {args.mode} {parameterized} -f {args.format} -o {output_filename} -s {i}" + \
                        (" -p" if args.prune else "")
                    print(command)
                    run(command.split(" "))
