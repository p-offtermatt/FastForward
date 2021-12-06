import argparse
import os
import twopower as tp


NET_TEMPLATE = r"""
PLACE
auxi,i,u,copy,rightlevel0,leftlevel0,{LEVEL_PLACES}f,auxf;

MARKING
auxi: 1;

TRANSITION tauxi
CONSUME auxi: 1;
PRODUCE i: {CHECK_NUM};

TRANSITION tauxf
CONSUME f: {CHECK_NUM};
PRODUCE auxf: 1;


TRANSITION tu
CONSUME i: 1;
PRODUCE u: 1, copy: 1;

TRANSITION t0l
CONSUME u: 1;
PRODUCE leftlevel0: 1;

TRANSITION b0l
CONSUME leftlevel0: 1;
PRODUCE u: 1;

TRANSITION t0r
CONSUME u: 1;
PRODUCE rightlevel0: 1;

TRANSITION b0r
CONSUME rightlevel0: 1;
PRODUCE u: 1;


{LEVELS}


TRANSITION tdec
CONSUME copy: 2;
PRODUCE f: 1, copy: 1;


{FINAL_TRANSITION}
"""

FINAL_TRANSITION_TEMPLATE = r"""
TRANSITION tf
CONSUME copy: 1, leftlevel{FINAL_LEVEL}: 1, rightlevel{FINAL_LEVEL}: 1;
PRODUCE f: 1;
"""

FORMULA_TEMPLATE = r"""
AGEF (auxf = 1)
"""


def GetNetAndFormulaForInstance(k, c):
    """
    Returns two string representations of a net and a formula in .lola format
    where invoking 1-soundness (that is, checking the formula) 
    checks for k-soundness
    of a net which is 2^(c+1), 2*(2^(c+1)), 3*(2^(c+1)), ... sound.
    """

    # build a net with c levels
    levels, places = tp.generate_twopower_levels(c)
    
    net_string = NET_TEMPLATE.replace("{LEVELS}", levels).replace("{LEVEL_PLACES}", places)
    final_transition = FINAL_TRANSITION_TEMPLATE.replace("{FINAL_LEVEL}", str(c)).replace("{SOUND_NUM}", str(pow(2, c+1)))
    net_string = net_string.replace("{FINAL_TRANSITION}", final_transition).replace("{CHECK_NUM}", str(k))


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

    for size in sizes:
        c = size
        sound_num = pow(2, c+1)
        for k in range(1, sound_num+1):
            net_string, formula_string = GetNetAndFormulaForInstance(k, c)
            out_filepath = args.output_dir + "/" + str(pow(2, c+1)) + "-reach_" + str(k) + "-check"

            print("Writing to " + out_filepath)

            with open(out_filepath + ".lola",
                'w') as net_file:
                net_file.write(net_string)
            with open(out_filepath + ".formula",
                'w') as formula_file:
                formula_file.write(formula_string)
        
        print("Done writing files for size " + str(c))
    print("Done with all sizes")
