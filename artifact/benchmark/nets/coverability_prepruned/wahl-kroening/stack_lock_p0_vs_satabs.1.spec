vars
l0 s0 s1 l1 l2 l3 l4 s3 l5 s4 l6 s2 l7 s5 l8 l9 l24 l10 l13 l11 l14 l15 l16 l17 l18 l19 l20 l21 l22 s8 l27 l23

rules
s1 >= 1,l5 >= 1 ->
s1' = s1-1,
l5' = l5-1,
s5' = s5+1,
l6' = l6+1;

s0 >= 1,l0 >= 1 ->
s0' = s0-1,
l0' = l0-1,
s1' = s1+1,
l1' = l1+1;

s0 >= 1,l17 >= 1 ->
s0' = s0+0,
l17' = l17-1,
l18' = l18+1;

s0 >= 1,l17 >= 1 ->
s0' = s0-1,
l17' = l17-1,
s1' = s1+1,
l18' = l18+1;

s1 >= 1,l2 >= 1 ->
s1' = s1+0,
l2' = l2-1,
l3' = l3+1;

s5 >= 1,l15 >= 1 ->
s5' = s5+0,
l15' = l15-1,
l16' = l16+1;

s5 >= 1,l9 >= 1 ->
s5' = s5-1,
l9' = l9-1,
s1' = s1+1,
l10' = l10+1;

s1 >= 1,l22 >= 1 ->
s1' = s1-1,
l22' = l22-1,
s8' = s8+1,
l27' = l27+1;

s5 >= 1,l6 >= 1 ->
s5' = s5+0,
l6' = l6-1,
l8' = l8+1;

s4 >= 1,l7 >= 1 ->
s4' = s4+0,
l7' = l7-1,
l9' = l9+1;

s5 >= 1,l19 >= 1 ->
s5' = s5+0,
l19' = l19-1,
l20' = l20+1;

s0 >= 1,l10 >= 1 ->
s0' = s0+0,
l10' = l10-1,
l11' = l11+1;

s0 >= 1,l24 >= 1 ->
s0' = s0+0,
l24' = l24-1,
l3' = l3+1;

s1 >= 1,l18 >= 1 ->
s1' = s1-1,
l18' = l18-1,
s5' = s5+1,
l19' = l19+1;

s1 >= 1,l10 >= 1 ->
s1' = s1+0,
l10' = l10-1,
l11' = l11+1;

s4 >= 1,l15 >= 1 ->
s4' = s4+0,
l15' = l15-1,
l16' = l16+1;

s3 >= 1,l4 >= 1 ->
s3' = s3-1,
l4' = l4-1,
s1' = s1+1,
l24' = l24+1;

s1 >= 1,l13 >= 1 ->
s1' = s1+0,
l13' = l13-1,
l14' = l14+1;

s4 >= 1,l6 >= 1 ->
s4' = s4+0,
l6' = l6-1,
l8' = l8+1;

s4 >= 1,l20 >= 1 ->
s4' = s4-1,
l20' = l20-1,
s0' = s0+1,
l21' = l21+1;

s0 >= 1,l2 >= 1 ->
s0' = s0-1,
l2' = l2-1,
s1' = s1+1,
l3' = l3+1;

s1 >= 1,l17 >= 1 ->
s1' = s1-1,
l17' = l17-1,
s0' = s0+1,
l18' = l18+1;

s0 >= 1,l0 >= 1 ->
s0' = s0+0,
l0' = l0-1,
l1' = l1+1;

s4 >= 1,l9 >= 1 ->
s4' = s4-1,
l9' = l9-1,
s0' = s0+1,
l10' = l10+1;

s0 >= 1,l18 >= 1 ->
s0' = s0-1,
l18' = l18-1,
s4' = s4+1,
l19' = l19+1;

s0 >= 1,l3 >= 1 ->
s0' = s0+0,
l3' = l3-1,
l4' = l4+1;

s2 >= 1,l4 >= 1 ->
s2' = s2-1,
l4' = l4-1,
s0' = s0+1,
l24' = l24+1;

s1 >= 1,l0 >= 1 ->
s1' = s1+0,
l0' = l0-1,
l1' = l1+1;

s0 >= 1,l13 >= 1 ->
s0' = s0+0,
l13' = l13-1,
l14' = l14+1;

s0 >= 1,l1 >= 1 ->
s0' = s0-1,
l1' = l1-1,
s1' = s1+1,
l2' = l2+1;

s1 >= 1,l10 >= 1 ->
s1' = s1+0,
l10' = l10-1,
l13' = l13+1;

s5 >= 1,l6 >= 1 ->
s5' = s5+0,
l6' = l6-1,
l7' = l7+1;

s1 >= 1,l4 >= 1 ->
s1' = s1-1,
l4' = l4+0,
s3' = s3+1,
l5' = l5+1;

s0 >= 1,l21 >= 1 ->
s0' = s0+0,
l21' = l21-1,
l22' = l22+1;

s0 >= 1,l5 >= 1 ->
s0' = s0-1,
l5' = l5-1,
s4' = s4+1,
l6' = l6+1;

s1 >= 1,l24 >= 1 ->
s1' = s1+0,
l24' = l24-1,
l3' = l3+1;

s0 >= 1,l14 >= 1 ->
s0' = s0-1,
l14' = l14-1,
s4' = s4+1,
l15' = l15+1;

s0 >= 1,l4 >= 1 ->
s0' = s0-1,
l4' = l4+0,
s2' = s2+1,
l5' = l5+1;

s1 >= 1,l17 >= 1 ->
s1' = s1+0,
l17' = l17-1,
l18' = l18+1;

s4 >= 1,l19 >= 1 ->
s4' = s4+0,
l19' = l19-1,
l20' = l20+1;

s5 >= 1,l16 >= 1 ->
s5' = s5-1,
l16' = l16-1,
s1' = s1+1,
l17' = l17+1;

s5 >= 1,l7 >= 1 ->
s5' = s5+0,
l7' = l7-1,
l9' = l9+1;

s0 >= 1,l22 >= 1 ->
s0' = s0+0,
l22' = l22-1,
l23' = l23+1;

s1 >= 1,l14 >= 1 ->
s1' = s1-1,
l14' = l14-1,
s5' = s5+1,
l15' = l15+1;

s4 >= 1,l8 >= 1 ->
s4' = s4+0,
l8' = l8-1,
l9' = l9+1;

s0 >= 1,l10 >= 1 ->
s0' = s0+0,
l10' = l10-1,
l13' = l13+1;

s5 >= 1,l20 >= 1 ->
s5' = s5-1,
l20' = l20-1,
s1' = s1+1,
l21' = l21+1;

s1 >= 1,l3 >= 1 ->
s1' = s1+0,
l3' = l3-1,
l4' = l4+1;

s1 >= 1,l0 >= 1 ->
s1' = s1-1,
l0' = l0-1,
s0' = s0+1,
l1' = l1+1;

s1 >= 1,l21 >= 1 ->
s1' = s1+0,
l21' = l21-1,
l22' = l22+1;

s4 >= 1,l16 >= 1 ->
s4' = s4-1,
l16' = l16-1,
s0' = s0+1,
l17' = l17+1;

s1 >= 1,l1 >= 1 ->
s1' = s1+0,
l1' = l1-1,
l2' = l2+1;

s5 >= 1,l8 >= 1 ->
s5' = s5+0,
l8' = l8-1,
l9' = l9+1;

s4 >= 1,l6 >= 1 ->
s4' = s4+0,
l6' = l6-1,
l7' = l7+1;

l0 >= 0 ->
l0' = l0+1;

init
l0=1, s0=1, s1=0, l1=0, l2=0, l3=0, l4=0, s3=0, l5=0, s4=0, l6=0, s2=0, l7=0, s5=0, l8=0, l9=0, l24=0, l10=0, l13=0, l11=0, l14=0, l15=0, l16=0, l17=0, l18=0, l19=0, l20=0, l21=0, l22=0, s8=0, l27=0, l23=0

target
s3>=0,l3>=0,s8>=1,l27>=1,l24>=0,l20>=0,l14>=0,l10>=0,s0>=0,l18>=0,l15>=0,l8>=0,l23>=0,l7>=0,l19>=0,s1>=0,l2>=0,l22>=0,l9>=0,s5>=0,l5>=0,s2>=0,l1>=0,l4>=0,l13>=0,s4>=0,l17>=0,l11>=0,l21>=0,l0>=0,l6>=0,l16>=0