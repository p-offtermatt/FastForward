LEVEL_PLACES_TEMPLATE=r"r{LEVEL},l{LEVEL},"

LEVEL_TEMPLATE = r"""
TRANSITION t{LEVEL}l
CONSUME l{PREV_LEVEL}: 1, r{PREV_LEVEL}: 1;
PRODUCE l{LEVEL}: 1;

TRANSITION b{LEVEL}l
CONSUME l{LEVEL}: 1;
PRODUCE l{PREV_LEVEL}: 1, r{PREV_LEVEL}: 1;

TRANSITION t{LEVEL}r
CONSUME l{PREV_LEVEL}: 1, r{PREV_LEVEL}: 1;
PRODUCE r{LEVEL}: 1;

TRANSITION b{LEVEL}r
CONSUME r{LEVEL}: 1;
PRODUCE l{PREV_LEVEL}: 1, r{PREV_LEVEL}: 1;
"""

def generate_twopower_levels(c):
    levels = ""
    places = ""
    for i in range(1, c+1):
        levels += LEVEL_TEMPLATE.replace("{LEVEL}", str(i)).replace("{PREV_LEVEL}", str(i-1))
        places += LEVEL_PLACES_TEMPLATE.replace("{LEVEL}", str(i))
        # add linebreaks for better readability
        levels += "\n\n"
    return levels,places