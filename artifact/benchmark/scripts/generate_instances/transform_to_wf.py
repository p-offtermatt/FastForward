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
            ending = ".lola"
        elif(path.endswith(".xml.tpn")):
            path_prefix = path[:-len(".xml.tpn")]
            folder_prefix, filename = path_prefix.rsplit("/", 1)
            ending = ".xml.tpn"
        else:
            print(f"Skipping {path}, does not have a known net extension.")
            continue
        
        
        command = f"dotnet fastforward/fastforward.dll transform-wf {path_prefix}{ending} -o {output_folder_name}/{filename}"
        print(command)
        sys.stdout.flush()
        try:
            output = subprocess.run(command, shell=True, timeout=60,stderr=subprocess.STDOUT, stdout=subprocess.PIPE)
            print(output)
        except subprocess.TimeoutExpired:
            print("Timed out...")
    print("Done!")
