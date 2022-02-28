import json
from shutil import Error

def strip_common_extensions(string):
    changed = True
    while changed:
        new_string = string
        new_string = removesuffix(new_string, ".lola")
        new_string = removesuffix(new_string, ".pnml")
        new_string = removesuffix(new_string, ".xml")
        new_string = removesuffix(new_string, ".spec")
        new_string = removesuffix(new_string, ".tts")

        changed = new_string != string
        string = new_string
    return string

def removesuffix(s, suf):
    if suf and s.endswith(suf):
        return s[:-len(suf)]
    return s

def get_entry_with_method_from_list(method, entries):
    possible_results = [x for x in entries if "methodName" in x and x["methodName"] == method]
    if len(possible_results) > 1:
        raise Error(f"Multiple possible results for method {method} in entries")
    if len(possible_results) == 0:
        return None
    return possible_results[0]

def read_json_from_file(filepath):
    with open(filepath, 'r') as json_file:
        print("Reading input from " + json_file.name)
        input_str = json_file.read()
        try:
            json_obj = json.loads(input_str)
        except json.decoder.JSONDecodeError:
            input_str = input_str + "]"
            json_obj = json.loads(input_str)
    return json_obj

def group_by_name(entries):
    result = dict()
    for entry in entries:
        normalized_sample_name = strip_common_extensions(str(entry["sampleName"]))
        entries_for_name = result.get(normalized_sample_name, list())
        entries_for_name.append(entry)
        result[normalized_sample_name] = entries_for_name

    # ensure all entrylists are ordered in the same way    
    for entrylist in result.values():
        entrylist.sort(key=lambda entry: entry["methodName"])

    return result

def get_time_from_entries(entry_list, default_time):
    result_list = list()
    for entry in entry_list:
        if "error" in entry:
            entry_time = default_time
        else:
            entry_time = entry["wallTime"]
        result_list.append(entry_time)
    return result_list
