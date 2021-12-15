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
CONSUME i: {REACH_NUM};
PRODUCE f: {REACH_NUM_MINUS_1};

TRANSITION tauxf
CONSUME f: {CHECK_NUM};
PRODUCE auxf: 1;
"""


FORMULA_TEMPLATE = r"""
AGEF (auxf = 1)
"""


def GetNetAndFormulaForInstance(check_num, reach_num):
    """
    Returns two string representations of a net and a formula in .lola format
    where invoking 1-soundness (that is, checking the formula) 
    checks for check_num-soundness
    of a net which is structurally-quasi-unsound.
    """
    
    net_string = NET_TEMPLATE.replace("{REACH_NUM}", str(reach_num)).replace("{REACH_NUM_MINUS_1}", str(reach_num-1)).replace("{CHECK_NUM}", str(check_num))

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

    for reach_num in sizes:
        for check_num in range(1, reach_num):
            net_string, formula_string = GetNetAndFormulaForInstance(check_num, reach_num)
            out_filepath = args.output_dir + "/" + str(reach_num) + "-sound_" + str(check_num) + "-check"

            print("Writing to " + out_filepath)

            with open(out_filepath + ".lola",
                'w') as net_file:
                net_file.write(net_string)
            with open(out_filepath + ".formula",
                'w') as formula_file:
                formula_file.write(formula_string)
        
        print("Done writing files for size " + str(reach_num))
    print("Done with all sizes")
