import argparse
import os
import utils

NET_TEMPLATE = r"""
PLACE
auxi,auxf,i,f,u,d;

MARKING
auxi: 1;

TRANSITION tauxi
CONSUME auxi: 1;
PRODUCE i: {CHECK_NUM};

TRANSITION ti
CONSUME i: 1;
PRODUCE u: 1, d: 1;

TRANSITION tu
CONSUME u: {REACH_NUM}, d: 1;
PRODUCE f: 1;

TRANSITION td
CONSUME d: 2;
PRODUCE d: 1, f: 1;

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
    of a net which reach_num,2*reach_num,... quasi-sound, but structurally unsound.
    """
    
    net_string = NET_TEMPLATE.replace("{REACH_NUM}", str(reach_num)).replace("{REACH_NUM_PLUS_1}", str(reach_num+1)).replace("{CHECK_NUM}", str(check_num))

    formula_string = FORMULA_TEMPLATE

    return net_string, formula_string

if __name__ == "__main__":
    parser = utils.generate_argparser()

    args = parser.parse_args()

    sizes = args.sizes
    os.makedirs(args.output_dir, exist_ok=True)

    for reach_num in sizes:
        for check_num in range(1, (reach_num+1) if not args.nocheck else 2):
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
