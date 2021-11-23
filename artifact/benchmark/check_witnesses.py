import argparse
import plotting_utils as utils
import subprocess


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('data_files', type=str, nargs='+',
                        help="One or more files that resulted from benchmarking and contain the witnesses to check.")

    args = parser.parse_args()

    for data_file in args.data_files:
        data = utils.read_json_from_file(data_file)

        for entry in data:

            if entry.get("error", "") != "":
                continue

            if "FastForward" not in entry["methodName"] and "LoLA" not in entry["methodName"]:
                continue

            witness = entry.get("path", "unreachable")
            if witness == "unreachable":
                continue

            command = f"dotnet fastforward/fastforward.dll witness-check {entry['netFile']} -f {entry['targetFile']} \"{witness}\""
            # command = f"dotnet fastforward/fastforward.dll witness-check {entry['netFile']} \"{witness}\""
            process = subprocess.run(
                command, shell=True,
                 stdout=subprocess.PIPE
                 )

            stdout = process.stdout.decode()

            if "Reached target marking: True" not in stdout:
                print("ERROR!")
                print(
                    f"Error on sample {entry['sampleName']} for method {entry['methodName']}: ")
                print("Last lines of output:")
                print(stdout.split("\n")[-10:])
                exit(12)

            print(
                f"Sample {entry['sampleName']} succeeded for method {entry['methodName']}")

        print("DONE, ALL SAMPLES SUCCEEDED!")
