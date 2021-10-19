Devices:
Display EPS1;
Display EPS3;
Display EPS6;
Display EPS8;
Display NEC-MT1065;
Canvas LW1;
Canvas LW2;
Canvas LW3;
Canvas LW4;
Canvas LW5;
Canvas LW6;
Canvas VD1;
Canvas VD2;
Document Doc1;
Document Doc2;
Lamp Lamp1;
Lamp Lamp2;
Lamp Lamp3;
Lamp Lamp4;
Lamp Lamp5;
Lamp Lamp6;

Actions:
cm1 Send2Disp [ ( Present 1 Document_1 | ) ] [ ( Sent2Disp 1 Display_1 Document_1 | ) ( Sent2Disp 0 Display_1 | Anyother_Document ) ];
cm2 ShowDoc [ ( Sent2Disp 1 Document_1 | EPS1 ) ( CanvasDown 1 | LW1 ) ] [ ( DocShown 1 Document_1 | LW1 ) ( DocShown 0 | Anyother_Document LW1 ) ];
cm3 ShowDoc [ ( Sent2Disp 1 Document_1 | EPS3 ) ( CanvasDown 1 | LW3 ) ] [ ( DocShown 1 Document_1 | LW3 ) ( DocShown 0 | Anyother_Document LW3 ) ];
cm4 ShowDoc [ ( Sent2Disp 1 Document_1 | EPS6 ) ( CanvasDown 1 | LW6 ) ] [ ( DocShown 1 Document_1 | LW6 ) ( DocShown 0 | Anyother_Document LW6 ) ];
cm5 ShowDoc [ ( Sent2Disp 1 Document_1 | EPS8 ) ( CanvasDown 1 | VD2 ) ] [ ( DocShown 1 Document_1 | VD2 ) ( DocShown 0 | Anyother_Document VD2 ) ];
cm6 ShowDoc [ ( Sent2Disp 1 Document_1 | NEC-MT1065 ) ( Pointing 1 Surface_1 | NEC-MT1065 ) ( CanvasDown 1 Surface_1 | ) ] [ ( DocShown 1 Document_1 Surface_1 | ) ( DocShown 0 Surface_1 | Anyother_Document ) ];
cm7 CanvasUp [ ( CanvasDown 1 | VD1 ) ] [ ( CanvasDown 0 | VD1 ) ];
cm8 CanvasUp [ ( CanvasDown 1 | VD2 ) ] [ ( CanvasDown 0 | VD2 ) ];
cm9 CanvasUp [ ( CanvasDown 1 | LW1 ) ] [ ( CanvasDown 0 | LW1 ) ];
cm10 CanvasUp [ ( CanvasDown 1 | LW2 ) ] [ ( CanvasDown 0 | LW2 ) ];
cm11 CanvasUp [ ( CanvasDown 1 | LW3 ) ] [ ( CanvasDown 0 | LW3 ) ];
cm12 CanvasUp [ ( CanvasDown 1 | LW4 ) ] [ ( CanvasDown 0 | LW4 ) ];
cm13 CanvasUp [ ( CanvasDown 1 | LW5 ) ] [ ( CanvasDown 0 | LW5 ) ];
cm14 CanvasUp [ ( CanvasDown 1 | LW6 ) ] [ ( CanvasDown 0 | LW6 ) ];
cm15 CanvasDown [ ( CanvasDown 0 | VD1 ) ] [ ( CanvasDown 1 | VD1 ) ];
cm16 CanvasDown [ ( CanvasDown 0 | VD2 ) ] [ ( CanvasDown 1 | VD2 ) ];
cm17 CanvasDown [ ( CanvasDown 0 | LW1 ) ] [ ( CanvasDown 1 | LW1 ) ];
cm18 CanvasDown [ ( CanvasDown 0 | LW2 ) ] [ ( CanvasDown 1 | LW2 ) ];
cm19 CanvasDown [ ( CanvasDown 0 | LW3 ) ] [ ( CanvasDown 1 | LW3 ) ];
cm20 CanvasDown [ ( CanvasDown 0 | LW4 ) ] [ ( CanvasDown 1 | LW4 ) ];
cm21 CanvasDown [ ( CanvasDown 0 | LW5 ) ] [ ( CanvasDown 1 | LW5 ) ];
cm22 CanvasDown [ ( CanvasDown 0 | LW6 ) ] [ ( CanvasDown 1 | LW6 ) ];
cm23 LightOn [ ( LightOn 0 | Lamp1 ) ] [ ( LightOn 1 | Lamp1 ) ];
cm24 LightOn [ ( LightOn 0 | Lamp2 ) ] [ ( LightOn 1 | Lamp2 ) ];
cm25 LightOn [ ( LightOn 0 | Lamp3 ) ] [ ( LightOn 1 | Lamp3 ) ];
cm26 LightOn [ ( LightOn 0 | Lamp4 ) ] [ ( LightOn 1 | Lamp4 ) ];
cm27 LightOn [ ( LightOn 0 | Lamp5 ) ] [ ( LightOn 1 | Lamp5 ) ];
cm28 LightOn [ ( LightOn 0 | Lamp6 ) ] [ ( LightOn 1 | Lamp6 ) ];
cm29 LightOff [ ( LightOn 1 | Lamp1 ) ] [ ( LightOn 0 | Lamp1 ) ];
cm30 LightOff [ ( LightOn 1 | Lamp2 ) ] [ ( LightOn 0 | Lamp2 ) ];
cm31 LightOff [ ( LightOn 1 | Lamp3 ) ] [ ( LightOn 0 | Lamp3 ) ];
cm32 LightOff [ ( LightOn 1 | Lamp4 ) ] [ ( LightOn 0 | Lamp4 ) ];
cm33 LightOff [ ( LightOn 1 | Lamp5 ) ] [ ( LightOn 0 | Lamp5 ) ];
cm34 LightOff [ ( LightOn 1 | Lamp6 ) ] [ ( LightOn 0 | Lamp6 ) ];
cm35 MoveProjector [ ( Pointing 0 Surface_1 | NEC-MT1065 ) ] [ ( Pointing 1 Surface_1 | NEC-MT1065 ) ( Pointing 0 | NEC-MT1065 Anyother_Surface ) ];
cm36 RemoveFromCanvas [ ( DocShown 1 Document_1 | LW1 ) ] [ ( DocShown 0 Document_1 | LW1 ) ];
cm37 RemoveFromCanvas [ ( DocShown 1 Document_1 | LW2 ) ] [ ( DocShown 0 Document_1 | LW2 ) ];
cm38 RemoveFromCanvas [ ( DocShown 1 Document_1 | LW3 ) ] [ ( DocShown 0 Document_1 | LW3 ) ];
cm39 RemoveFromCanvas [ ( DocShown 1 Document_1 | LW4 ) ] [ ( DocShown 0 Document_1 | LW4 ) ];
cm40 RemoveFromCanvas [ ( DocShown 1 Document_1 | LW5 ) ] [ ( DocShown 0 Document_1 | LW5 ) ];
cm41 RemoveFromCanvas [ ( DocShown 1 Document_1 | LW6 ) ] [ ( DocShown 0 Document_1 | LW6 ) ];
cm42 RemoveFromCanvas [ ( DocShown 1 Document_1 | VD1 ) ] [ ( DocShown 0 Document_1 | VD1 ) ];
cm43 RemoveFromCanvas [ ( DocShown 1 Document_1 | VD2 ) ] [ ( DocShown 0 Document_1 | VD2 ) ];

Percepts:
( Pointing 1 LW5 NEC-MT1065 );

Goals:
( LightOn 1 Lamp1 );
( LightOn 1 Lamp2 );
( DocShown 1 Doc1 LW3 );
( DocShown 1 Doc2 LW1 );
( CanvasDown 1 VD1 );
