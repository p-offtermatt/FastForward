vars
    v1 v2

rules
    v1 >= 1 ->
		    v1' = v1-1,
		    v2' = v2+1;


init
    v1 = 1, v2 = 0

target
    v2 >= 1
