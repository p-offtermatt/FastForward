import argparse
import plotting_utils as utils


def remove_usual_file_endings(input: str):
    while True:
        if input.endswith(".xml"):
            input = input[:-(len(".xml"))]
        elif input.endswith(".lola"):
            input = input[:-(len(".lola"))]
        elif input.endswith(".tpn"):
            input = input[:-(len(".tpn"))]
        else:
            return input

def print_statistics(entries):
    print("Printing statistics...")

    smallBoundProps = [entry for entry in entries if "smallBoundProperties" in entry and entry["smallBoundProperties"] != None]
    tuples = [(prop["smallBoundProperties"]["A_n"], prop["smallBoundProperties"]["minTime"], prop["smallBoundProperties"]["maxTime"]) for prop in smallBoundProps]
    stripped_tuples = {(tuples.count(entry),) + entry for entry in set(tuples)}
    
    # for entry in stripped_tuples:
    #     print(entry)

    ans = [prop["smallBoundProperties"]["A_n"]/prop["transitions"] for prop in smallBoundProps]
    counted_ans = [(an, ans.count(an)) for an in set(ans)]
    counted_ans.sort(key = lambda x: x[0])

    for range in [(0,0.75), (0.75,1), (1, 1.00000000000001), (1.00000000000001, 1.75), (1.75, 1000)]:
        total = sum([an[1] for an in counted_ans if range[0] <= an[0] < range[1]])
        print(f"{range}: {total}")

    print(f"Max: {max([an[0] for an in counted_ans])}")
    print(f"Min: {min([an[0] for an in counted_ans if an[0] > 0])}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('-i', '--inputfiles', nargs="+", required=True)

    args = parser.parse_args()

    original_entries = []

    # read input
    for filepath in args.inputfiles:
        original_entries += utils.read_json_from_file(filepath)

    print_statistics(original_entries)
