import json
import os
import sys
from subprocess import Popen, PIPE, CalledProcessError, TimeoutExpired
import time
import benchmark_utils

timeout_time = 120


if __name__ == "__main__":
    folder_name = sys.argv[1]
    output_filepath = sys.argv[2]

    with open(output_filepath, 'w+') as output_file:

        output_file.write("[")
        output_file.flush()
        first = True
        for entry in os.scandir(folder_name):
            path = entry.path
            print("----" + path + "----")
            if path.endswith(".lola"):
                ending = ".lola"
            elif path.endswith(".xml"):
                ending = ".xml"
            else:
                continue

            if not first:
                output_file.write(",\n")
            first = False

            path_prefix = path[:-len(ending)]
            folder_prefix, filename = path_prefix.rsplit("/", 1)

            command = f"dotnet fastforward/fastforward.dll soundness-reverseTransitions {path_prefix}{ending} -k 1"
            process = Popen(command.split(" "), stdout=PIPE,
                            stderr=PIPE, preexec_fn=benchmark_utils.limit_virtual_memory)
            try:
                print(command)
                execution_time = time.time()
                result, stderr = process.communicate(timeout=timeout_time)
                result_obj = json.loads(result)
            except CalledProcessError:
                execution_time = time.time() - execution_time
                process.kill()
                result, stderr = process.communicate()

                result_obj = {"error": stderr.decode(
                    "utf-8").replace("\"", "'")}
            except TimeoutExpired:
                execution_time = time.time() - execution_time
                result_obj = {"error": "timeout"}
                process.kill()
                result, stderr = process.communicate(timeout=timeout_time)

                print("Timeout!")
            except json.JSONDecodeError as e:
                process.kill()
                print("Encountered an error:")
                print(e.msg)
                print(stderr.decode("utf-8"))
                print(result.decode("utf-8"))
                result_obj = {"error": repr(
                    e) + ", " + stderr.decode("utf-8".replace("\"", "'"))}
            result_obj["file"] = path

            output_file.write(json.dumps(result_obj))
            output_file.flush()

        output_file.write("]")
        output_file.flush()
