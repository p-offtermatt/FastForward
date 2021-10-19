import csv
import pandas
import sys
import json
from tabulate import tabulate

times_seen = {}

default_time = 600000


def count_solved_instances(data):
    error_sources = {}
    times_solved = {}
    samples_seen = {}

    for row in data:
        name = row["sampleName"]
        samples_seen[name] = samples_seen.get(name, 0) + 1
        if "error" in row and row["error"] != "":
            if row["error"] == "timeout":
                errors_entry = error_sources.get(name, {})
                errors_entry["timeout"] = errors_entry.get("timeout", 0) + 1
                error_sources[name] = errors_entry
            else:
                errors_entry = error_sources.get(name, {})
                errors_entry["other_errors"] = errors_entry.get(
                    "other_errors", 0) + 1
                error_sources[name] = errors_entry
        else:
            times_solved[name] = times_solved.get(name, 0) + 1

    return samples_seen, times_solved, error_sources


def simplify_data(data, retranslated):
    result = {}
    for row in data:
        name = row["sampleName"]
        if retranslated:
            sampleName = name.rsplit("_", 1)[0].rstrip("lsz")
            appliedMethod = name.rsplit("_", 1)[1]
            completeName = sampleName + appliedMethod
        else:
            completeName = name

        times_seen[completeName] = times_seen.get(completeName, 0) + 1

        result[completeName] = row


def rolling_average(old_average, count, new_value):
    return old_average * (count-1)/count + new_value * 1/count


def group_entries(entries, function):
    group_to_entries = {}
    for entry in entries:
        group_name = function(entry)
        group_to_entries[group_name] = group_to_entries.get(
            group_name, []) + [entry]
    return group_to_entries


def calculate_column_average(entries, column_name, default_value=None, error_column=None):
    result = 0
    count_entries_with_column = 0
    for entry in entries:
        try:
            new_value = entry[column_name]
        except KeyError:  # if key not found, entry doesn't contain column
            if default_value is None:
                continue
            else:
                new_value = default_value

        # column is present, but has no value
        if new_value is None or new_value == "":
            if default_value is None:
                continue
            else:
                new_value = default_value

        # the "error" column is present and has an entry, which means we ignore the column entry and insert the default value, or just ignore it if none is given
        if error_column is not None:
            try:
                error_value = entry[error_column]
                if error_value != "":
                    if default_value is None:
                        continue
                    else:
                        new_value = default_value
            except KeyError:
                pass
        count_entries_with_column += 1
        result = rolling_average(result, count_entries_with_column, new_value)
    return result


def pretty_print_solved_instances(times_seen_dict, times_solved_dict, errors_dict):
    total_instances = 0
    solved_by_method = {}
    solved_by_translation_level = {
        "old_translation": {},
        "l": {},
        "s": {},
        "z": {}
    }
    instances_seen_of_translation_level = {}
    print_single_samples = False

    for entry_name in set([name.rsplit("_", 1)[0] for name in times_seen_dict]):
        total_instances += times_seen_dict[entry_name + "_markingEQ"]

        solved_by_method["markingEQ"] = solved_by_method.get("markingEQ", 0) + \
            times_solved_dict.get(entry_name + "_markingEQ", 0)

        solved_by_method["qReachability"] = solved_by_method.get("qReachability", 0) + \
            times_solved_dict.get(entry_name + "_qReachability", 0)

        solved_by_method["markingEQ+Pruning"] = solved_by_method.get("markingEQ+Pruning", 0) + \
            times_solved_dict.get(entry_name + "_markingEQ+Pruning", 0)

        solved_by_method["qReachability+Pruning"] = solved_by_method.get("qReachability+Pruning", 0) + \
            times_solved_dict.get(entry_name + "_qReachability+Pruning", 0)

        def add_entry_to_translation_level_dict(entry_name, times_solved_dict, translation_level):
            solved_by_translation_level[translation_level]["markingEQ"] = \
                solved_by_translation_level[translation_level].get("markingEQ", 0) + \
                times_solved_dict.get(entry_name + "_markingEQ", 0)

            solved_by_translation_level[translation_level]["qReachability"] = \
                solved_by_translation_level[translation_level].get("qReachability", 0) + \
                times_solved_dict.get(entry_name + "_qReachability", 0)

            solved_by_translation_level[translation_level]["markingEQ+Pruning"] = \
                solved_by_translation_level[translation_level].get("markingEQ+Pruning", 0) + \
                times_solved_dict.get(entry_name + "_markingEQ+Pruning", 0)

            solved_by_translation_level[translation_level]["qReachability+Pruning"] = \
                solved_by_translation_level[translation_level].get("qReachability+Pruning", 0) + \
                times_solved_dict.get(entry_name + "_qReachability+Pruning", 0)

        if entry_name[len(entry_name) - 1] in "1234567890":
            instances_seen_of_translation_level["old_translation"] = \
                instances_seen_of_translation_level.get("old_translation", 0) \
                + times_seen_dict[entry_name + "_markingEQ"]
            add_entry_to_translation_level_dict(
                entry_name, times_solved_dict, "old_translation")
        elif entry_name[len(entry_name) - 2] in "1234567890" and entry_name[len(entry_name) - 1] in "zls":
            translation_level = entry_name[len(entry_name) - 1]
            instances_seen_of_translation_level[translation_level] = \
                instances_seen_of_translation_level.get(translation_level, 0) \
                + times_seen_dict[entry_name + "_markingEQ"]
            add_entry_to_translation_level_dict(
                entry_name[:-1], times_solved_dict, translation_level)
        else:
            print("COULD NOT DETERMINE TRANSLATION LEVEL FOR " + entry_name)

        if print_single_samples:
            print(f"Sample: {entry_name}")
            print(f"Solved:")
            print("MarkingEQ: {}/{}".format(
                times_solved_dict.get(entry_name + "_markingEQ", 0), times_seen_dict[entry_name + "_markingEQ"]))
            print("QReachability: {}/{}".format(
                times_solved_dict.get(entry_name + "_qReachability", 0),
                times_seen_dict[entry_name + "_qReachability"]))
            print("MarkingEQ+Pruning: {}/{}".format(
                times_solved_dict.get(entry_name + "_markingEQ+Pruning", 0),
                times_seen_dict[entry_name + "_markingEQ+Pruning"]))
            print("QReachability+Pruning: {}/{}".format(
                times_solved_dict.get(
                    entry_name + "_qReachability+Pruning", 0),
                times_seen_dict[entry_name + "_qReachability+Pruning"]))
            print("<---->")

    print("Total:")
    print(f"{total_instances} instances seen")
    print(f"Solved by method: ------")
    print("Solved by QReachability: {}/{}".format(
        solved_by_method.get("qReachability", 0), total_instances))
    print("Solved by MarkingEQ: {}/{}".format(solved_by_method.get("markingEQ", 0), total_instances))
    print("Solved by QReachability+Pruning: {}/{}".format(solved_by_method.get("qReachability+Pruning", 0),
                                                          total_instances))
    print("Solved by MarkingEQ+Pruning: {}/{}".format(
        solved_by_method.get("markingEQ+Pruning", 0), total_instances))

    for translation_level in ["old_translation", "l", "s", "z"]:
        print(f"Total instances of translation level '{translation_level}':" +
              str(instances_seen_of_translation_level[translation_level]))
        print(f"Solved by method: ------")
        print("Solved by QReachability: {}/{}".
              format(solved_by_translation_level[translation_level].get("qReachability", 0),
                     instances_seen_of_translation_level[translation_level]))
        print("Solved by MarkingEQ: {}/{}".
              format(solved_by_translation_level[translation_level].get("markingEQ", 0),
                     instances_seen_of_translation_level[translation_level]))
        print("Solved by QReachability+Pruning: {}/{}".
              format(solved_by_translation_level[translation_level].get("qReachability+Pruning", 0),
                     instances_seen_of_translation_level[translation_level]))
        print("Solved by MarkingEQ+Pruning: {}/{}".
              format(solved_by_translation_level[translation_level].get("markingEQ+Pruning", 0),
                     instances_seen_of_translation_level[translation_level]))


if __name__ == "__main__":
    filename = sys.argv[1]
    retranslated = len(sys.argv) > 2 and sys.argv[2] == "-retts"
    with open(filename, 'r') as input_file:
        input_str = input_file.read()
        try:
            json_obj = json.loads(input_str)
        except json.decoder.JSONDecodeError:
            input_str = input_str.replace("\'", "\"")
            try:
                json_obj = json.loads(input_str)
            except json.decoder.JSONDecodeError:
                input_str = input_str + "]"
                json_obj = json.loads(input_str)

        #samples_seen, times_solved, error_sources = count_solved_instances(json_obj)
        #pretty_print_solved_instances(samples_seen, times_solved, error_sources)

        # data = simplify_data(json_obj, retranslated)
        grouped_by_method_name = group_entries(
            json_obj, lambda entry: entry["methodName"])

        result_table = []
        for method_name, entries in grouped_by_method_name.items():
            result_table.append([method_name, str(calculate_column_average(entries, "wallTime", None, "error"))])

        result_table = [
            entry for entry in result_table if not "backwards" in entry[0]]
        result_table.sort(key=lambda entry: float(entry[1]))
        print(tabulate(result_table, headers=[
              "method", "time without timeouts"]))

        print("\n--------")
        print("Solved instances per method:")

        for method_name, entries in grouped_by_method_name.items():
            print(f"{method_name} solved {len([entry for entry in entries if entry.get('error', '') == ''])} out of {len(entries)} instances")

        print("----------\n")
        print(f"{len(grouped_by_method_name)} different methods, {len(json_obj)} entries, {len(json_obj) / len(grouped_by_method_name)} benchmarks per method")