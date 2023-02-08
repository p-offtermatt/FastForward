NAME (null)
ROWS
 N  OBJ
 G  c0      
 G  c1      
 G  c2      
 G  c3      
 G  c4      
 G  c5      
 G  init_final_diff_positive
 L  init_final_diff_negative
 L  deadlock_transition_t1_place_init
 L  deadlock_transition_t2_place_a
 L  deadlock_transition_t2_place_b
COLUMNS
    MARKER    'MARKER'                 'INTORG'
    I0(1)     c0        1
    I0(1)     init_final_diff_positive  -1
    I0(1)     init_final_diff_negative  -1
    I0(1)     deadlock_transition_t1_place_init  -1
    I0(1)     deadlock_transition_t2_place_a  1
    I1(2)     c0        -1
    I1(2)     c2        1
    I1(2)     init_final_diff_positive  1
    I1(2)     init_final_diff_negative  1
    I1(2)     deadlock_transition_t2_place_a  -1
    B2(1)     c3        1
    B3(1)     c3        1
    B4(1)     c4        1
    B5(1)     c5        1
    B6(1)     c5        1
    MARKER    'MARKER'                 'INTEND'
RHS
    RHS1      c3        1
    RHS1      c4        1
    RHS1      c5        1
    RHS1      init_final_diff_positive  1
    RHS1      init_final_diff_negative  -1
    RHS1      deadlock_transition_t1_place_init  0
    RHS1      deadlock_transition_t2_place_a  0
    RHS1      deadlock_transition_t2_place_b  0
BOUNDS
 LI BND1      I0(1)     0
 LI BND1      I1(2)     0
 BV BND1      B2(1)   
 BV BND1      B3(1)   
 BV BND1      B4(1)   
 BV BND1      B5(1)   
 BV BND1      B6(1)   
INDICATORS
 IF init_final_diff_positive  B2(1)     1
 IF init_final_diff_negative  B3(1)     1
 IF deadlock_transition_t1_place_init  B4(1)     1
 IF deadlock_transition_t2_place_a  B5(1)     1
 IF deadlock_transition_t2_place_b  B6(1)     1
ENDATA
