import json

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