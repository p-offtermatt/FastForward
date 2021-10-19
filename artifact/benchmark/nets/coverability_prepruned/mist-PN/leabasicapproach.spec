vars
unlockS unlockC Swhile Cwhile Sbefore Cbefore lockS Sbad lockC Cbad Sin Cin Safterin Send Cafterin Cend

rules
unlockS >= 1,Cbad >= 1 ->
unlockS' = unlockS-1,
Cbad' = Cbad-1,
lockS' = lockS+1,
Cin' = Cin+1;

unlockS >= 1,Sbefore >= 1 ->
unlockS' = unlockS-1,
Sbefore' = Sbefore-1,
lockS' = lockS+1,
Sbad' = Sbad+1;

lockC >= 1,Sin >= 1 ->
lockC' = lockC-1,
Sin' = Sin-1,
unlockC' = unlockC+1,
Safterin' = Safterin+1;

unlockC >= 1,Cbefore >= 1 ->
unlockC' = unlockC-1,
Cbefore' = Cbefore-1,
lockC' = lockC+1,
Cbad' = Cbad+1;

lockS >= 1,Safterin >= 1 ->
lockS' = lockS-1,
Safterin' = Safterin-1,
unlockS' = unlockS+1,
Send' = Send+1;

lockC >= 1,Cafterin >= 1 ->
lockC' = lockC-1,
Cafterin' = Cafterin-1,
unlockC' = unlockC+1,
Cend' = Cend+1;

lockS >= 1,Cin >= 1 ->
lockS' = lockS-1,
Cin' = Cin-1,
unlockS' = unlockS+1,
Cafterin' = Cafterin+1;

Cend >= 1 ->
Cend' = Cend-1,
Cwhile' = Cwhile+1;

unlockC >= 1,Sbad >= 1 ->
unlockC' = unlockC-1,
Sbad' = Sbad-1,
lockC' = lockC+1,
Sin' = Sin+1;

Swhile >= 1 ->
Swhile' = Swhile-1,
Sbefore' = Sbefore+1;

Cwhile >= 1 ->
Cwhile' = Cwhile-1,
Cbefore' = Cbefore+1;

Send >= 1 ->
Send' = Send-1,
Swhile' = Swhile+1;

Swhile >= 0 ->
Swhile' = Swhile+1;

Cwhile >= 0 ->
Cwhile' = Cwhile+1;

init
unlockS=1, unlockC=1, Swhile=1, Cwhile=1, Sbefore=0, Cbefore=0, lockS=0, Sbad=0, lockC=0, Cbad=0, Sin=0, Cin=0, Safterin=0, Send=0, Cafterin=0, Cend=0

target
unlockS>=0,lockS>=0,Cwhile>=0,Send>=0,Swhile>=0,Cend>=0,Sin>=0,Safterin>=0,lockC>=0,Cbad>=1,Cbefore>=0,Cin>=0,Sbad>=1,Cafterin>=0,unlockC>=0,Sbefore>=0
