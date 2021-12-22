import argparse


def generate_argparser():
    parser = argparse.ArgumentParser()
    parser.add_argument('output_dir', type=str,
            help="The directory to which the output files should be written.")
    parser.add_argument("-s", "--sizes", nargs="+", type=int)
    parser.add_argument("-nc", "--nocheck",
help="""If set to true, will only generate the c-sound_1-check.lola file for each size;
otherwise generates c-sound_k-check.lola for k=1 to k=c.
""", default=False, action='store_true')

    return parser