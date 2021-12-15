import argparse
import os

NET_TEMPLATE = r"""
PLACE
auxi,auxf,i,f;

MARKING
auxi: 1;

TRANSITION tauxi
CONSUME auxi: 1;
PRODUCE i: {CHECK_NUM};

TRANSITION t
CONSUME i: {SOUND_NUM};
PRODUCE f: {SOUND_NUM};

TRANSITION tauxf
CONSUME f: {CHECK_NUM};
PRODUCE auxf: 1;
"""


FORMULA_TEMPLATE = r"""
AGEF (auxf = 1)
"""


def GetNetAndFormulaForInstance(check_num, sound_num):
    """
    Returns two string representations of a net and a formula in .lola format
    where invoking 1-soundness (that is, checking the formula) 
    checks for check_num-soundness
    of a net which is sound_num, 2*sound_num, 3*sound_num, ... sound.
    """
    
    net_string = NET_TEMPLATE.replace("{SOUND_NUM}", str(sound_num)).replace("{CHECK_NUM}", str(check_num))

    formula_string = FORMULA_TEMPLATE

    return net_string, formula_string

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('output_dir', type=str,
            help="The directory to which the output files should be written.")
    parser.add_argument("-s", "--sizes", nargs="+", type=int)

    args = parser.parse_args()

    sizes = args.sizes
    os.makedirs(args.output_dir, exist_ok=True)

    for sound_num in sizes:
        for check_num in range(1, sound_num):
            net_string, formula_string = GetNetAndFormulaForInstance(check_num, sound_num)
            out_filepath = args.output_dir + "/" + str(sound_num) + "-sound_" + str(check_num) + "-check"

            print("Writing to " + out_filepath)

            with open(out_filepath + ".lola",
                'w') as net_file:
                net_file.write(net_string)
            with open(out_filepath + ".formula",
                'w') as formula_file:
                formula_file.write(formula_string)
        
        print("Done writing files for size " + str(sound_num))
    print("Done with all sizes")
