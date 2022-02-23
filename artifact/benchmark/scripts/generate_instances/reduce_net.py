import argparse
import shutil
import os

def call_and_log(command):
    print(command)
    os.system(command)

if __name__ == "__main__":
    prom_home = os.getenv("PROM_HOME")
    if prom_home is None:
        raise FileNotFoundError("PROM_HOME is not set! Make sure the env variable PROM_HOME points to the ProM installation!")
    prom_input = prom_home + "/net.pnml"
    prom_output = prom_home + "/reduced.pnml"



    parser = argparse.ArgumentParser()
    parser.add_argument('input_file')

    args = parser.parse_args()

    input_file = args.input_file
    input_file_no_extensions, extension = os.path.splitext(input_file)

    print(input_file)
    
    call_and_log("dotnet ../fastforward/fastforward.dll translate-wf " + input_file + " -f CGraph -o tmp -m Reachability")
    print("Removing " + input_file)
    os.remove(input_file)
    

    shutil.rmtree('xml', ignore_errors=True)
    os.makedirs('xml')

    call_and_log("../tools/Hadara_AdSimul_Red tmp.xml")

    if not os.path.isfile("xml/red_tmpxml.xml"):
        print("File was solved by reducing, moving on")
        exit(0)

    call_and_log(f"dotnet ../fastforward/fastforward.dll translate-wf xml/red_tmpxml.xml -m Reachability -f PNML -o {prom_home}/net")
    call_and_log(f"cd {prom_home}; sh ProM_CLI.sh -f call_reduction.java")

    call_and_log(f"dotnet ../fastforward/fastforward.dll translate-wf {prom_home}/reduced.pnml -m Reachability -f Lola -o " + input_file_no_extensions)

    shutil.rmtree('xml', ignore_errors=True)
    os.remove("tmp.xml")
    shutil.rmtree('xml', ignore_errors=True)