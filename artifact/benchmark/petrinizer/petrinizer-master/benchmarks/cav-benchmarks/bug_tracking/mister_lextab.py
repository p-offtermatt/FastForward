# mister_lextab.py. This file automatically created by PLY (version 3.4). Don't edit!
_tabversion   = '3.2'
_lextokens    = {'PRIME': 1, 'TARGET': 1, 'VARS': 1, 'RULES': 1, 'INVARIANTS': 1, 'NUMBER': 1, 'INIT': 1, 'ARROW': 1, 'IN': 1, 'TRUE': 1, 'ID': 1, 'GTE': 1}
_lexreflags   = 0
_lexliterals  = '=,;[]+-'
_lexstateinfo = {'INITIAL': 'inclusive'}
_lexstatere   = {'INITIAL': [("(?P<t_ID>[a-zA-Z_][a-zA-Z0-9_]*)|(?P<t_NUMBER>\\d+)|(?P<t_newline>\\n+)|(?P<t_ignore_COMMENT>\\#.*)|(?P<t_ARROW>\\->)|(?P<t_PRIME>\\')|(?P<t_GTE>>=)", [None, ('t_ID', 'ID'), ('t_NUMBER', 'NUMBER'), ('t_newline', 'newline'), (None, None), (None, 'ARROW'), (None, 'PRIME'), (None, 'GTE')])]}
_lexstateignore = {'INITIAL': ' \t'}
_lexstateerrorf = {'INITIAL': 't_error'}
