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
20 If Dist(P_Curr,P_lakkozo1_uj)<=0.1 Then
21 'If Dist(P_Curr,P_lakkozo1)<=0.1 Then
22 mpoz=8
23 EndIf
24 If Dist(P_Curr,P_lakkozo2)<=0.1 Then 'Or Dist(P_Curr,P_lakkozo2_uj)<=0.1 Then
25 mpoz=10
26 EndIf
27 If Dist(P_Curr,P_szalag_uj)<=0.1 Then
28 mpoz=12
29 EndIf
30 If Dist(P_Curr,P_szfelett_uj)<=0.1 Then
31 mpoz=13
32 EndIf
33 'If Dist(P_Curr,P_service1)<=0.1 Then
34 'mpoz=14
35 'EndIf
36 'If Dist(P_Curr,P_service2)<=0.1 Then
37 'mpoz=15
38 'EndIf
39 M_Outw(10480)=mpoz
P_home=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
P_feszek=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
P_hajl1berak=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
P_hajl1kivesz=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
P_hajl2berak=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
P_hajl2kivesz=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
P_lakkozo1_uj=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
P_lakkozo2=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
P_szalag_uj=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
P_szfelett_uj=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
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
