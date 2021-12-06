LEVEL_PLACES_TEMPLATE=r"rightlevel{LEVEL},leftlevel{LEVEL},"

LEVEL_TEMPLATE = r"""
TRANSITION t{LEVEL}l
CONSUME leftlevel{PREV_LEVEL}: 1, rightlevel{PREV_LEVEL}: 1;
PRODUCE leftlevel{LEVEL}: 1;

TRANSITION b{LEVEL}l
CONSUME leftlevel{LEVEL}: 1;
PRODUCE leftlevel{PREV_LEVEL}: 1, rightlevel{PREV_LEVEL}: 1;

TRANSITION t{LEVEL}r
CONSUME leftlevel{PREV_LEVEL}: 1, rightlevel{PREV_LEVEL}: 1;
PRODUCE rightlevel{LEVEL}: 1;

TRANSITION b{LEVEL}r
CONSUME rightlevel{LEVEL}: 1;
PRODUCE leftlevel{PREV_LEVEL}: 1, rightlevel{PREV_LEVEL}: 1;
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