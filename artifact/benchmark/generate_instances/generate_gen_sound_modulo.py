import argparse
import os


NET_TEMPLATE = r"""
PLACE
auxi,i,a,b,c,f,auxf;

MARKING
auxi: 1;

TRANSITION auxInit
CONSUME auxi: 1;
PRODUCE i: {check-num};

TRANSITION t1
CONSUME i: 1;
PRODUCE a: 1, b: 1;

TRANSITION t2
CONSUME b: 1;
PRODUCE a: {unsound-num};

TRANSITION t3
CONSUME a: {unsound-num};
PRODUCE c: 1;

TRANSITION t4
CONSUME a: 1, c: 1;
PRODUCE f: 1;

TRANSITION auxFinal
CONSUME f: {check-num};
PRODUCE auxf: 1;
"""

FORMULA_TEMPLATE = r"""
AGEF (auxi=0 AND a = 0 AND b = 0 AND i = 0 AND f = 0 AND auxf = 1)
"""


def GetNetAndFormulaForInstance(k, c):
    """
    Returns two string representations of a net and a formula in .lola format
    where invoking 1-soundness (that is, checking the formula) 
    checks for k-soundness
    of an enclosed net, and the enclosed net is 1...c-1  sound and c... unsound.
    """

    net_string = NET_TEMPLATE.replace("{check-num}", str(k)).replace("{unsound-num}", str(c))
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
        for k in range(1, c+1):
            net_string, formula_string = GetNetAndFormulaForInstance(k, c)
            out_filepath = args.output_dir + "/" + str(c) + "-unsound_" + str(k) + "-check"

            print("Writing to " + out_filepath)

            with open(out_filepath + ".lola",
                'w') as net_file:
                net_file.write(net_string)
            with open(out_filepath + ".formula",
                'w') as formula_file:
                formula_file.write(formula_string)

            if k == c:
                break
        
        print("Done writing files for size " + str(c))
    print("Done with all sizes")
