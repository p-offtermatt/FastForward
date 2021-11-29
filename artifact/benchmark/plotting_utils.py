import json

tool_to_label = {
    "BFC": r"\textsc{Bfc}",
    "ICover": r"\textsc{ICover}",
    "MIST": r"\textsc{mist}",
    "LoLA": r"\textsc{LoLA}",
    "FastForward_zero+Pruning+Competitive": r"\textsc{Dijkstra}",
    "FastForward_QMarkingEQGurobi+Pruning+Competitive": r"\textsc{FF(\astar, $\mathcal{D}_{\Q}$)}",
    "FastForward_GBFS_QMarkingEQGurobi+Pruning+Competitive": r"\textsc{FF(GBFS, $\mathcal{D}_{\Q}$)}",

    "FastForward a-starzero+Pruning": r"\textsc{Dijkstra}",
    "FastForward a-starQMarkingEQGurobi+Pruning": r"\textsc{FF(\astar, $\mathcal{D}_{\Q}$)}",
    "FastForward best-firstQMarkingEQGurobi+Pruning": r"\textsc{FF(GBFS, $\mathcal{D}_{\Q}$)}",
}

tool_to_symbol = {
    "BFC": "asterisk",

    "QCover": "star",
    "ICover": "star",


    "MIST-Backward": "x",
    "MIST": "x",

    "Lola": "+",
    "LoLA": "+",

    "FastForward GBFS QMarkingEQGurobi+Competitive": "square*",
    "FastForward GBFS QMarkingEQGurobi+Pruning": "square*",
    "FastForward GBFS QMarkingEQGurobi": "square*",


    "a-star+QMarkingEQGurobi+Pruning": "square*",
    "a-star+QMarkingEQGurobi": "square",
    "a-star+NMarkingEQGurobi+Pruning": "triangle*",
    "a-star+structuralperplace-n+Pruning": "diamond*",
    "best-first+QMarkingEQGurobi+Pruning": "oplus*",
    "best-first+QMarkingEQGurobi": "oplus",
    "best-first+NMarkingEQGurobi+Pruning": "otimes*",
    "best-first+structuralperplace-n+Pruning": "*",

    "FastForward a-starzero+Pruning": "*",
    "FastForward a-starQMarkingEQGurobi+Pruning": "square",
    "FastForward best-firstQMarkingEQGurobi+Pruning": "square*",

    "FastForward QMarkingEQGurobi+Pruning+Competitive": "diamond*",
    "FastForward QMarkingEQGurobi+Competitive": "diamond*",
    "FastForward QMarkingEQGurobi+Pruning": "diamond*",
    "FastForward QMarkingEQGurobi": "diamond*",


    "FastForward zero+Pruning+Competitive": "*",
    "FastForward zero+Competitive": "*",
    "FastForward zero+Pruning": "*",
    "FastForward zero": "*",


    "FastForward_QMarkingEQGurobi+Pruning+Competitive": "diamond*",
    "FastForward_QMarkingEQGurobi+Competitive": "diamond*",

    "FastForward_zero+Pruning+Competitive": "*",
    "FastForward_zero+Competitive": "*",

    "KReach": "o",
    "Kosaraju": "o",
}


def get_tool_label(tool: str):
    return tool_to_label.get(tool, tool)


def get_tool_mark(tool: str):
    return tool_to_symbol.get(tool, '+')


def extract_pathlength_from_entry(entry):
    if entry.get("error", "") != "" or entry.get("path", "unreachable") == "unreachable":
        return None
    if "bfc" in entry["methodName"].lower():
        # discard initial marking, final marking, and final null marking
        return len(entry["path"].split(";")) - 3
    if "mist" in entry["methodName"].lower():
        return len(entry["path"].split("> ["))
    if "kreach" in entry["methodName"].lower():
        return None
    if "qcover" in entry["methodName"].lower() or "icover" in entry["methodName"].lower():
        return None
    if "lola" in entry["methodName"].lower() or "fastforward" in entry["methodName"].lower():
        length = len([x for x in entry["path"].split(
            ", ") if not x.startswith("t_")])
        return length

    print("Did not know how to parse methodName " + entry["methodName"])
    exit(54)

def entry_fits_consensus(must_be_reachable, entry, consensus_data):
    fitting_entries = [tmp for tmp in consensus_data if tmp["sampleName"] == entry["sampleName"]]

    if len(fitting_entries) == 0:
        print("No consensus entry for sample " +  entry["sampleName"] + ", skipping it")
        return False
    
    fitting_entry = fitting_entries[0]

    if fiting_entry["consensus"] == "unclear":
        return False

    return (fitting_entry["consensus"] == "reachable" if must_be_reachable else fitting_entry["consensus"] != "reachable")


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

def get_shortest_path_for_sample(sample_name, data):
    sample_entries = [
        entry for entry in data if entry["sampleName"] == sample_name]

    baseline_entries = [
        entry for entry in sample_entries if "QMarkingEQGurobi" in entry["methodName"] and not "GBFS" in entry["methodName"]]

    baseline_lengths = [(entry["methodName"], extract_pathlength_from_entry(
        entry)) for entry in baseline_entries if extract_pathlength_from_entry(
        entry) is not None]

    if len({length for name, length in baseline_lengths if length is not None}) > 1:
            print("Different baseline lengths for " + sample)
            exit(12)

    if not baseline_lengths:
        return None

    baseline_length = baseline_lengths[0][1]
    return baseline_length

    
