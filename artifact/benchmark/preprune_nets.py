import os
import sys
import subprocess

timeout = 60

if __name__ == "__main__":
    folder_name = sys.argv[1]
    output_folder_name = sys.argv[2]

    for entry in os.scandir(folder_name):
        path = entry.path
        if(path.endswith(".lola")):
            path_prefix = path[:-len(".lola")]
            folder_prefix, filename = path_prefix.rsplit("/", 1)

            for out_format in ["Lola", "Dotspec"]:
                command = f"dotnet fastforward/fastforward.dll translate -f {out_format} --forward-prune -o {output_folder_name}/{filename} {path_prefix}.lola {path_prefix}.formula"
                print(command)
                try:
                    subprocess.run(command, shell=True, timeout=60)
                except subprocess.TimeoutExpired:
                    print("Timed out...")
