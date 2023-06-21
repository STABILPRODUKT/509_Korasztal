1 mpoz=0
2 If Dist(P_Curr,P_home)<=1 Then
3 mpoz=1
4 EndIf
5 If Dist(P_Curr,P_feszek)<=0.1 Then
6 mpoz=2
7 EndIf
8 If Dist(P_Curr,P_hajl1berak)<=0.1 Then
9 mpoz=4
10 EndIf
11 If Dist(P_Curr,P_hajl1kivesz)<=0.1 Then
12 mpoz=5
13 EndIf
14 If Dist(P_Curr,P_hajl2berak)<=0.1 Then
15 mpoz=6
16 EndIf
17 If Dist(P_Curr,P_hajl2kivesz)<=0.1 Then
18 mpoz=7
19 EndIf
20 If Dist(P_Curr,P_lakkozo1)<=0.1 Then
21 mpoz=8
22 EndIf
23 If Dist(P_Curr,P_lakkozo2)<=0.1 Then
24 mpoz=10
25 EndIf
26 If Dist(P_Curr,P_szalag)<=0.1 Then
27 mpoz=12
28 EndIf
29 If Dist(P_Curr,P_szalag_felett)<=0.1 Then
30 mpoz=13
31 EndIf
32 M_Outw(10480)=mpoz
P_home=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
P_feszek=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
P_hajl1berak=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
P_hajl1kivesz=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
P_hajl2berak=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
P_hajl2kivesz=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
P_lakkozo1=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
P_lakkozo2=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
P_szalag=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
P_szalag_felett=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
Pfeszek=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
Phajl1berak=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
Phajl1kivesz=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
Phajl2berak=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
Phajl2kivesz=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
Phome=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
Plakkozo1=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
Plakkozo2=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
Pszalag=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
Pszalag_felett=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
