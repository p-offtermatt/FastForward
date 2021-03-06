vars
x2 x13 x3 x21 x0 x10 x14 x22 x4 x23 x15 x24 x5 x25 x16 x27 x19 x11 x20 x12 x6 x7 x29 x9 x8 x28 x26 x1 x17 x18 x30

rules
x4 >= 1 ->
x4' = x4-1,
x0' = x0+1,
x10' = x10+1;

x13 >= 1,x21 >= 1 ->
x13' = x13-1,
x21' = x21-1,
x14' = x14+1,
x22' = x22+1;

x0 >= 1,x20 >= 1 ->
x0' = x0-1,
x20' = x20-1,
x13' = x13+1;

x16 >= 1 ->
x16' = x16-1,
x19' = x19+1;

x17 >= 1 ->
x17' = x17-1,
x18' = x18+1;

x3 >= 1,x22 >= 1 ->
x3' = x3-1,
x22' = x22-1,
x4' = x4+1,
x23' = x23+1;

x10 >= 1 ->
x10' = x10-1,
x2' = x2+1;

x16 >= 1,x28 >= 1 ->
x16' = x16-1,
x28' = x28-1,
x19' = x19+1,
x26' = x26+1;

x6 >= 1 ->
x6' = x6-1,
x7' = x7+1,
x29' = x29+1;

x15 >= 1 ->
x15' = x15-1,
x11' = x11+1,
x20' = x20+1;

x18 >= 1 ->
x18' = x18-1,
x11' = x11+1,
x20' = x20+1;

x2 >= 1 ->
x2' = x2-1,
x3' = x3+1,
x21' = x21+1;

x7 >= 1 ->
x7' = x7-1,
x9' = x9+1;

x3 >= 1 ->
x3' = x3-1,
x0' = x0+1,
x10' = x10+1;

x17 >= 1 ->
x17' = x17-1,
x16' = x16+1,
x30' = x30+1;

x18 >= 1 ->
x18' = x18-1,
x19' = x19+1;

x6 >= 1 ->
x6' = x6-1,
x8' = x8+1,
x28' = x28+1;

x7 >= 1,x30 >= 1 ->
x7' = x7-1,
x30' = x30-1,
x6' = x6+1;

x14 >= 1 ->
x14' = x14-1,
x11' = x11+1,
x20' = x20+1;

x20 >= 1 ->
x20' = x20+0,
x11' = x11+1;

x8 >= 1,x26 >= 1 ->
x8' = x8-1,
x26' = x26-1,
x0' = x0+1,
x10' = x10+1;

x14 >= 1,x23 >= 1 ->
x14' = x14-1,
x23' = x23-1,
x15' = x15+1,
x24' = x24+1;

x4 >= 1,x24 >= 1 ->
x4' = x4-1,
x24' = x24-1,
x5' = x5+1,
x25' = x25+1;

x0 >= 1,x19 >= 1 ->
x0' = x0-1,
x19' = x19-1,
x12' = x12+1,
x13' = x13+1;

x20 >= 1 ->
x20' = x20-1,
x13' = x13+1;

x9 >= 1,x11 >= 1 ->
x9' = x9-1,
x11' = x11-1,
x1' = x1+1,
x2' = x2+1;

x10 >= 1 ->
x10' = x10+0,
x0' = x0+1;

x16 >= 1,x29 >= 1 ->
x16' = x16-1,
x29' = x29-1,
x17' = x17+1;

x10 >= 1,x12 >= 1 ->
x10' = x10-1,
x12' = x12-1,
x2' = x2+1;

x10 >= 1,x11 >= 1 ->
x10' = x10-1,
x11' = x11-1,
x2' = x2+1;

x19 >= 1 ->
x19' = x19-1,
x11' = x11+1,
x20' = x20+1;

x5 >= 1,x27 >= 1 ->
x5' = x5-1,
x27' = x27-1,
x6' = x6+1;

x1 >= 1,x20 >= 1 ->
x1' = x1-1,
x20' = x20-1,
x13' = x13+1;

x9 >= 1 ->
x9' = x9-1,
x0' = x0+1,
x10' = x10+1;

x15 >= 1,x25 >= 1 ->
x15' = x15-1,
x25' = x25-1,
x16' = x16+1,
x27' = x27+1;

x8 >= 1 ->
x8' = x8-1,
x9' = x9+1;

init
x2=1, x13=1, x3=0, x21=0, x0=0, x10=0, x14=0, x22=0, x4=0, x23=0, x15=0, x24=0, x5=0, x25=0, x16=0, x27=0, x19=0, x11=0, x20=0, x12=0, x6=0, x7=0, x29=0, x9=0, x8=0, x28=0, x26=0, x1=0, x17=0, x18=0, x30=0

target
x1>=0,x6>=0,x9>=0,x3>=0,x17>=0,x12>=1,x19>=0,x10>=0,x11>=0,x20>=0,x21>=1,x25>=0,x23>=1,x26>=0,x13>=0,x18>=0,x4>=0,x27>=0,x28>=1,x7>=0,x16>=0,x8>=0,x0>=0,x29>=0,x15>=0,x24>=0,x2>=0,x14>=0,x22>=0,x30>=1,x5>=0
