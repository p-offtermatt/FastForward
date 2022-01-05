import argparse
import os
import pathlib
import random
from subprocess import run


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("inputdir")
    parser.add_argument("outputdir")

    args = parser.parse_args()

    filepaths = [os.path.join(args.inputdir, filename) for filename in os.listdir(args.inputdir) if not filename.startswith("A")]
    print(f"Found {len(filepaths)} nets from libraries B and C in {args.inputdir}")

    pathlib.Path(args.outputdir).mkdir(parents=True, exist_ok=True)

    repeat_num = 50
    chain_num = 40
    print(f"Generating {repeat_num} nets by combining {chain_num} random nets")

    for i in range(0, repeat_num):
        indices = random.choices(range(0, len(filepaths)), k=chain_num)
        nets = [filepaths[index] for index in indices]
        name = "_".join([str(index) for index in indices])
        output_name = name
        output_path = os.path.join(args.outputdir, output_name)

        command = f"dotnet ../fastforward/fastforward.dll chain-nets {' '.join(nets)} -f Lola -o {output_path}"
        print(command)
        run(command.split(" "))
    


    
    
