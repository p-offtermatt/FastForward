vars
s0 s1 l0 l1 l2 l3 l4 l5 l6 l7 l8 l9 l10 l11 l12 l13 l14 l15 l16 l17 l18 l19 l20 l21 l22 l23 l24 l25 

rules
l0>=1, s0>=0, s1>=4 -> 
	l0'=l0-1,
	l1'=l1+1;

l0>=1, s0>=0, s1>=4 -> 
	l0'=l0-1,
	l2'=l2+1;

l1>=1, s0>=0, s1>=4 -> 
	l1'=l1-1,
	l5'=l5+1;

l2>=1, s0>=0, s1>=4 -> 
	l2'=l2-1,
	l3'=l3+1;

l3>=1, s0>=0, s1>=4 -> 
	s0'=s0+2,
	s1'=s1-2,
	l3'=l3-1,
	l4'=l4+1;

l5>=1, s0>=0, s1>=4 -> 
	l5'=l5-1,
	l6'=l6+1;

l6>=1, s0>=0, s1>=4 -> 
	l6'=l6-1,
	l7'=l7+1;

l7>=1, s0>=0, s1>=4 -> 
	l7'=l7-1,
	l8'=l8+1;

l7>=1, s0>=0, s1>=4 -> 
	l7'=l7-1,
	l9'=l9+1;

l8>=1, s0>=0, s1>=4 -> 
	l8'=l8-1,
	l6'=l6+1;

l9>=1, s0>=0, s1>=4 -> 
	l9'=l9-1,
	l10'=l10+1;

l10>=1, s0>=0, s1>=4 -> 
	l10'=l10-1,
	l11'=l11+1;

l10>=1, s0>=0, s1>=4 -> 
	l10'=l10-1,
	l12'=l12+1;

l11>=1, s0>=0, s1>=4 -> 
	l11'=l11-1,
	l15'=l15+1;

l12>=1, s0>=0, s1>=4 -> 
	l12'=l12-1,
	l13'=l13+1;

l13>=1, s0>=0, s1>=4 -> 
	s0'=s0+2,
	s1'=s1-2,
	l13'=l13-1,
	l14'=l14+1;

l15>=1, s0>=0, s1>=4 -> 
	l15'=l15-1,
	l16'=l16+1;

l16>=1, s0>=0, s1>=4 -> 
	l16'=l16-1,
	l17'=l17+1;

l16>=1, s0>=0, s1>=4 -> 
	l16'=l16-1,
	l18'=l18+1;

l17>=1, s0>=0, s1>=4 -> 
	l17'=l17-1,
	l19'=l19+1;

l18>=1, s0>=0, s1>=4 -> 
	l18'=l18-1,
	l19'=l19+1;

l19>=1, s0>=0, s1>=4 -> 
	l19'=l19-1,
	l9'=l9+1;

l19>=1, s0>=0, s1>=4 -> 
	l19'=l19-1,
	l20'=l20+1;

l20>=1, s0>=0, s1>=4 -> 
	l20'=l20-1,
	l21'=l21+1;

l20>=1, s0>=0, s1>=4 -> 
	l20'=l20-1,
	l23'=l23+1;

l21>=1, s0>=0, s1>=4 -> 
	s0'=s0+4,
	s1'=s1-4,
	l21'=l21-1,
	l25'=l25+1;

l22>=1, s0>=0, s1>=4 -> 
;

l23>=1, s0>=0, s1>=4 -> 
;

l4>=1, s0>=2, s1>=2 -> 
	s0'=s0-2,
	s1'=s1+2,
	l4'=l4-1,
	l5'=l5+1;

l14>=1, s0>=2, s1>=2 -> 
	s0'=s0-2,
	s1'=s1+2,
	l14'=l14-1,
	l15'=l15+1;


init
l0>=1, s0=0, s1=4

target
s0>=4, s1>=0,l25>=1

invariants
s0=1, s1=1
