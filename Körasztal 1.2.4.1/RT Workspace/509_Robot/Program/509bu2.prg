1 '*************************      509 Körasztal      *************************
2 '***************************************************************************
3 'P1-P4 paletta pontok - nem kell már
4 'PLACE - palettázás - nem kell
5 'PDestination és PEscape - ütközáskor használjuk
6 'Pfészek - megfogási pont
7 'Phajl1_elott - számolt poz (Pszor)
8 'Phajl1berak - Mindegyik terméknél egyforma
9 'Phajl1_2_vez1 és 2 - alsó megfogó megfog és lehúz
10 'Phajl1kivesz - utolsó pont, ahol a felsõ pofa megfog
11 'Plakkozó_elott - számolt poz (PSzor)
12 'PSzor - eltolás
13 'P_xxx - nem felvett pontok, értéket kapnak
14 '***************************************************************************
15 'DEKLARÁCIÓ
16 '***************************************************************************
17 '
18 Dim mpa(10)               'paraméter tömb
19 Dim mres(9)               'eredmény tömb
20 Dim prout(15)             'pontlista
21 Dim mrout(15,15)          'rout tábla
22 'Def Plt 1,P1,P2,P3,  ,24,1,1
23 'P1 <Start Point>         Refers to the pallet's start point.
24 'P2 <End Point A>         One of the ending points for the pallet. Transit point of arc for arc pallet.
25 'P3 <End Point B>         Another ending point for the pallet. Ending point of arc for arc pallet.
26 '***************************************************************************
27 'ALAPHELYZET
28 '***************************************************************************
29 Accel 70,70
30 Spd 100000&
31 Ovrd 5
32 HOpen 1
33 HOpen 2
34 For mi=0 To 11
35 M_Outw(10160+mi*16)=0       'Kimenetek inicializálása
36 Next mi
37 mcmd_cnt=0                              'parancs számláló
38 mcmd_num=0                            'parancs szám
39 mcmd=0                                    'parancs
40 mrouter=1                                 'root táblázat méret
41 mrstat=0                                   'root táblát inicializálni kell/nemkell
42 mdis=1                                      'távolság a legközelebbi ponttól
43 '***************************************************************************
44 'ÜTKÖZÉSVÉDELEM
45 '***************************************************************************
46 Def Act 5,M_ColSts(1)=1 GoTo *deepimpact,S
47 Act 5=1
48 ColChk On, NOErr
49 '***************************************************************************
50 'Vészleállás
51 '***************************************************************************
52 EStop=M_Inw(10544)
53 Def Act 2,M_Inw(10544)>0 GoTo *emergencystop,S
54 Act 2=1
55 '***************************************************************************
56 'FÖPROGRAM CIKLUS
57 '***************************************************************************
58 While 1
59 Jmargit=PtoJ(PHome)
60 '
61 '***************************************************************************
62 'Pozíciók
63 '***************************************************************************
64 '***************************************************************************
65 '
66 'Phajl1berak.Z = Phajl1berak.Z +(M_Inw(10560)/1000)
67 'Phajl2berak.Z = Phajl2berak.Z +(M_Inw(10576)/1000)
68 'Plakkozo1.Z = Plakkozo1.Z +(M_Inw(10592)/1000)
69 'Plakkozo2.Z = Plakkozo2.Z +(M_Inw(10608)/1000)
70 P_service1 = Pservice1
71 P_service2 = Pservice2
72 P_home = PHome
73 P_feszek = Pfeszek
74 P_hajl1berak = Phajl1berak
75 P_hajl1kivesz = Phajl1kivesz
76 P_hajl2berak =  Phajl2berak
77 P_hajl2kivesz = Phajl2kivesz
78 P_lakkozo1 = Plakkozo1
79 P_lakkozo1_uj = Plakkozo1_uj
80 P_lakkozo2 = Plakkozo2
81 P_lakkozo2_uj = Plakkozo2_uj
82 'P_szalag = Pszalag
83 P_szalag_uj = Pszalag_uj
84 'P_szalag_felett = Pszalag_felett
85 P_szfelett_uj = Psz_felett_uj
86 P_hajl1berak.Z = Phajl1berak.Z +(M_Inw(10560)/1000)
87 P_hajl2berak.Z = Phajl2berak.Z +(M_Inw(10576)/1000)
88 P_lakkozo1.Z = Plakkozo1.Z +(M_Inw(10592)/1000)
89 P_lakkozo2.Z = Plakkozo2.Z +(M_Inw(10608)/1000)
90 P_hajl1_elott = Phajl1kivesz * PSzor
91 'P_hajl1_elott = Phajl1_elott
92 P_hajl2_elott = P_hajl2kivesz * PSzor
93 'P_hajl2_elott = Phajl2_elott
94 P_lakkozo1_elott = Plakkozo1 * PSzor
95 P_lakkozo2_elott = Plakkozo2 * PSzor
96 '***************************************************************************
97 '***************************************************************************
98 mcmd_num=M_Inw(10160)                               'parancsszám beolvasása
99 seb=M_Inw(10480)               ' Sebesség a plc-tõl
100 mNR= M_Inw(10512)          '  M_Inw(10482)****Paletta mód amíg nincs a helyén az asztal****
101 'PLACE = Plt 1,mNR           '****Paletta mód amíg nincs a helyén az asztal****
102 '***************************************************************************
103 If mcmd_num <> 0 Then
104 '
105      mcode = -1                                                      'visszatérési érték -1
106 '
107     If mcmd_num > mcmd_cnt Then
108         For mi=1 To 10
109             mpa(mi)=0                                               'paraméterek törlése
110         Next mi
111     '
112         For mi=1 To 9
113             mres(mi)=0                                              'eredmények törlése
114         Next mi
115     '
116         mfinish=0                                                     'subrutin lefutott jel nullázása
117         mcmd_cnt=mcmd_num                                 'parancs számláló növelése
118         mcmd=M_Inw(10176)                                   'parancs beolvasása
119         For mi=1 To 10
120             mpa(mi)=M_Inw(10176+mi*16)                 'paraméterek beolvasása
121         Next mi
122     '
123         mstate=1
124         GoSub *statemachine                                    'ugrás az állapotgépre
125     Else
126         GoSub *statemachine
127     EndIf
128     '
129     If mfinish=1 Then
130          For mi=1 To 9
131              M_Outw(10192+mi*16)=mres(mi)            'eredmények visszaküldése
132          Next mi
133          M_Outw(10192)=mcode                               'hibakód visszaküldése
134          M_Outw(10176)=mcmd                                'parancs visszaküldése
135          M_Outw(10160)=mcmd_num                        'parancsszám visszaküldése
136          Dly 0.1
137          mfinish=0
138          mcmd=0
139          mstate=0
140     EndIf
141 '
142 Else
143 '
144      mcmd_cnt=0                                                    'ha parancsszám 0 -> parancs számláló 0
145 '
146 EndIf
147 '
148 WEnd
149 '
150 '***************************************************************************
151 'Állapotgép
152 '***************************************************************************
153 *statemachine
154 Select mcmd                                                'parancs végrehajtás, szubrutin hívások
155 Case 0
156              Dly 0.05
157              Break
158 Case 2
159              GoSub *chopen                                  'megfogó nyit
160              Break
161 Case 3
162              GoSub *chclose                                 'megfogó zár
163              Break
164 Case 6
165              GoSub *cgohome                                 'alaphelyzetbe áll
166              Break
167 Case 7
168              GoSub *cgoservice                                 'szervíz helyzetbe áll
169              Break
170 Case 20
171              GoSub *cmoveautofeszek
172              Break
173 Case 21
174              GoSub *cmoveautohajl2be
175              Break
176 Case 22
177              GoSub *cmoveautohajl2ki
178              Break
179 Case 23
180              GoSub *cmoveautohajl1be
181              Break
182 Case 24
183              GoSub *cmoveautohajl1ki
184              Break
185 Case 25
186              GoSub *cmoveautolakk2be
187              Break
188 Case 26
189              GoSub *cmoveautolakk2ki
190              Break
191 Case 27
192              GoSub *cmoveautolakk1be
193              Break
194 Case 28
195              GoSub *cmoveautolakk1ki
196              Break
197 Case 30
198              GoSub *cmoveautoszalag
199              Break
200 Case 31
201              GoSub  *cmoveautohaj2ki2
202              Break
203 Case 32
204              GoSub  *cmoveautohaj1ki2
205              Break
206 Case 33
207              GoSub  *cmoveautohaj2ki4
208              Break
209 Case 34
210              GoSub  *cmoveautohaj1ki4
211              Break
212 Case 40
213              GoSub *cmovefromstation
214              Break
215 Case 41
216              GoSub *cmove2position                          'mozgás célkoordinátákhoz
217              Break
218 Case 51                                                     'abort parancs
219              GoSub *cabort
220              Break
221 Default
222              mfinish=1
223              Break
224 End Select
225 Return
226 '
227 '***************************************************************************
228 'ÜTKÖZÉS
229 '***************************************************************************
230 *deepimpact
231 ColChk Off
232 Servo On
233 Ovrd 5
234 PEscape = P_ColDir(1) * (-50)     ' új     visszavonulási irány kalkuláció (eddigi mozgással ellentétesen kb. 5 mm)
235 PDestination = P_Fbc(1) + PEscape' új
236 Mvs PDestination' új
237 '*HOpen 1 régi
238 '*Mov P_Curr, -20 régi
239 Servo Off
240 Error 9100
241 '***************************************************************************
242 'E Stop
243 '***************************************************************************
244 *emergencystop
245 Servo Off
246 Error 9100
247 '***************************************************************************
248 'MEGFOGO NYIT
249  '******************************************************************************
250 *chopen
251 '
252 Select mstate
253 Case 1
254               If ((mpa(1) >=1) And (mpa(1)<=2)) Then
255                        mstate=mstate+1
256               Else
257                        mcode= -2
258                        mfinish=1
259               EndIf
260               Break
261 Case 2
262                HOpen mpa(1)
263                mstate=mstate+1
264                Break
265 Case 3
266                If M_In(900+(mpa(1)-1)) = 1 Then
267                       Dly 0.1
268                       mstate=mstate+1
269                EndIf
270                Break
271 Case 4
272                mres(1)=mpa(1)
273                mcode=0
274                mfinish=1
275                Break
276 End Select
277 '
278 Return
279 '
280 '***************************************************************************
281 'MEGFOGO ZÁR
282  '******************************************************************************
283 *chclose
284 '
285 Select mstate
286 Case 1
287               If ((mpa(1)>=1) And (mpa(1)<=2)) Then
288                        mstate=mstate+1
289               Else
290                        mcode= -2
291                        mfinish=1
292               EndIf
293               Break
294 Case 2
295                HClose mpa(1)
296                mstate=mstate+1
297                Break
298 Case 3
299                If M_In(900+(mpa(1)-1)) = 0 Then
300                    Dly 0.1
301                    mstate=mstate+1
302                EndIf
303                Break
304 Case 4
305                mres(1)=mpa(1)
306                mcode=0
307                mfinish=1
308                Break
309 End Select
310 '
311 Return
312 '
313 '******************************************************************************
314 *cgohome
315 '
316 Select mstate
317 Case 1
318               If ((mpa(1)>=1) And (mpa(1)<=2)) Then
319                        mstate=mstate+1
320               Else
321                        mcode= -2
322                        mfinish=1
323               EndIf
324               Break
325 Case 2
326                HOpen 1
327                HOpen 2
328                If (M_In(900) = 1 And M_In(901) = 1) Then
329                    mstate=4
330                EndIf
331                Break
332 Case 3
333                'HOpen 2
334                'If M_In(901) = 1 Then
335                    mstate=mstate+1
336                'EndIf
337                'Break
338 Case 4
339                If Dist(P_Curr,PHome)<5 Then
340                    Mov PHome
341                    mstate=8
342                Else
343                    mstate=mstate+1
344                EndIf
345                Break
346 Case 5
347                If mpa(1)=1 Then Ovrd seb
348                If mpa(1)=2 Then Ovrd seb
349                mstate=mstate+1
350                Break
351 Case 6
352                GoSub *initwhere
353                GoSub *findnearest                               'legközelebbi pont megkeresése
354                If mdis>10 Then                                  'ha a távolság nagyobb mint 10mm
355                    ptmp = P_Curr
356                    ptmp.X = prout(mnpi).X
357                    ptmp.Y = prout(mnpi).Y
358                    ptmp.Z = prout(mnpi).Z
359                    Mvs ptmp
360                    Mov prout(mnpi)                               'mozgás a legközelebbi pontba
361                EndIf
362                mstate=mstate+1
363                Break
364 Case 7
365                mdsti = 1                                          'célpont index
366                GoSub *route                                       'pontról pontra lépkedés a root táblában
367                mstate=mstate+1
368                Break
369 Case 8
370                mcode=0
371                mfinish=1
372                Break
373 End Select
374 '
375 Return
376 '
377 '******************************************************************************
378 'SZERVÍZ POZÍCIÓ
379 '******************************************************************************
380 *cgoservice
381 Select mstate
382 Case 1
383                 Select mpa(1)
384                     Case 1
385                         Ovrd 10
386                         Cnt 1
387                         Mov Pservice1
388                         Mov Pservice1, 100
389                         Cnt 0
390                     Case 2
391                         Ovrd 10
392                         Cnt 1
393                         Mov Pservice2
394                         Mov Pservice2, 100
395                         Cnt 0
396                 End Select
397                 mres(1)=mpa(1)
398                 mcode=0
399                 mfinish=1
400 End Select
401 '
402 Return
403 '
404 '******************************************************************************
405 'MOZGÁS TESZT
406 '******************************************************************************
407 *cmove2position
408 '
409 Select mstate
410 Case 1
411                  If (mpa(1)>=1) And (mpa(1)<=13) Then
412                     mstate=mstate+1
413                  Else
414                     mcode= -2
415                     mfinish=1
416                  EndIf
417                  Break
418 Case 2
419                  Select mpa(1)
420                     Case 1
421                         mstate=3
422                     Case 2
423                         mstate=4
424                     Case 3
425                         mstate=5
426                     Case 4
427                         mstate=6
428                     Case 5
429                         mstate=7
430                     Case 6
431                         mstate=8
432                     Case 7
433                         mstate=9
434                     Case 8
435                         mstate=10
436                     Case 9
437                         mstate=11
438                     Case 10
439                         mstate=12
440                     Case 11
441                         mstate=13
442                     Case 12
443                         mstate=14
444                     Case 13
445                         mstate=15
446                  End Select
447                  Break
448 Case 3
449                  Ovrd seb
450                  Fine 100
451                  Mov PHome
452                  mstate=16
453                  Break
454 Case 4
455                  Ovrd seb
456                  Mov Pfeszek,-150
457                  Fine 100
458                  Mov Pfeszek
459                  mstate=16
460                  Break
461 Case 5
462                  Ovrd seb
463                  Mov Pfeszek_felett,-170
464                  Fine 100
465                  Mov Pfeszek_felett
466                  mstate=16
467                  Break
468 Case 6
469                  Ovrd seb
470                  Mov P_hajl1berak,-150
471                  Fine 100
472                  Mvs P_hajl1berak
473                  mstate=16
474                  Break
475 Case 7
476                  Ovrd seb
477                  Mov Phajl1kivesz,-150
478                  Fine 100
479                  Mov Phajl1kivesz
480                  mstate=16
481                  Break
482 Case 8
483                  Ovrd seb
484                  Mov P_hajl2berak,-150
485                  Fine 100
486                  Mvs P_hajl2berak
487                  mstate=16
488                  Break
489 Case 9
490                  Ovrd seb
491                  Mov Phajl2kivesz,-150
492                  Fine 100
493                  Mov Phajl2kivesz
494                  mstate=16
495                  Break
496 Case 10
497                  Ovrd seb
498                  Fine 100
499                  Mov P_lakkozo1,-150
500                  Fine 100
501                  Mvs P_lakkozo1
502                  mstate=16
503                  Break
504 Case 11
505                  Ovrd seb
506                  Fine 100
507                  Mov Plakkozo1_elott
508                  mstate=16
509                  Break
510 Case 12
511                  Ovrd seb
512                  Fine 100
513                  Mov P_lakkozo2,-150
514                  Mvs P_lakkozo2
515                  mstate=16
516                  Break
517 Case 13
518                  Ovrd seb
519                  Fine 100
520                  Mov Plakkozo2_elott
521                  mstate=16
522                  Break
523 Case 14
524                  Ovrd seb
525                  Fine 100
526                  Mov Pszalag
527                  mstate=16
528                  Break
529 Case 15
530                  Ovrd seb
531                  Fine 100
532                  Mov Pszalag_felett
533                  mstate=16
534                  Break
535 Case 16
536                  mres(1)=mpa(1)
537                  mcode=0
538                  mfinish=1
539                  Break
540 End Select
541 '
542 Return
543 '
544 '******************************************************************************
545 *cabort
546 '
547 mstate=0
548 mcode=0
549 mfinish=1
550 '
551 Return
552 '
553 '******************************************************************************
554 *cmoveautofeszek
555 '
556     Ovrd seb
557     Mov Pfeszek,-20
558     Fine 100
559     Mov Pfeszek
560     HClose 2
561     Dly 0.1
562     Fine 0
563     mcode=0
564     mfinish=1
565 '
566 Return
567 '
568 '******************************************************************************
569 *cmoveautohajl2be
570 '
571     Ovrd seb
572     Cnt 1
573     Mov Pfeszek,-150
574     Mov PHome
575     Mvs Phajl2_elott
576     Cnt 0
577     Fine 100
578     Mvs P_hajl2berak
579     Fine 0
580     mcode=0
581     mfinish=1
582 '
583 Return
584 '
585 '******************************************************************************
586 *cmoveautohajl2ki
587 '
588     Ovrd seb
589     Cnt 1
590     If Dist(P_Curr,P_home)>0.1 Then
591     Mov P_Curr,-150
592     EndIf
593     Mov Phajl2kivesz,-150
594     Cnt 0
595     Fine 100
596     Mov Phajl2kivesz
597     Fine 0
598     HClose 2
599     Dly 0.1
600     mcode=0
601     mfinish=1
602 '
603 Return
604 '
605 '******************************************************************************
606 *cmoveautohajl1be
607 '
608     Ovrd seb
609     Cnt 1
610     Mov Pfeszek,-150
611     Mov PHome
612  Ovrd (seb/1.5)
613     Mov Phajl1_elott
614     Cnt 0
615     Fine 200
616     Mvs P_hajl1berak
617     Fine 0
618  Ovrd seb
619     mcode=0
620     mfinish=1
621 '
622 Return
623 '
624 '******************************************************************************
625 *cmoveautohajl1ki
626 '
627     Ovrd seb
628     Cnt 1
629     If Dist(P_Curr,P_home)>0.1 Then
630     Mov P_Curr,-150
631     EndIf
632     Mov Phajl1kivesz,-150
633     Cnt 0
634     Fine 100
635     Mov Phajl1kivesz
636     Fine 0
637     HClose 2
638     Dly 0.1
639     mcode=0
640     mfinish=1
641 '
642 Return
643 '
644 '******************************************************************************
645 *cmoveautolakk2be
646 '
647     Ovrd seb
648     Cnt 1
649     If Dist(P_Curr,P_home)>0.1 Then
650     Mov P_Curr,-150
651     EndIf
652     Mov Plakkozok_kozott
653     Mov P_lakkozo2, -150
654     Cnt 0
655     Fine 200
656     Mvs P_lakkozo2
657     Fine 0
658     mcode=0
659     mfinish=1
660 '
661 Return
662 '
663 '******************************************************************************
664 *cmoveautolakk2ki
665 '
666     Ovrd seb
667     Cnt 1
668     If Dist(P_Curr,P_home)>0.1 Then
669     Mov P_Curr,-150
670     EndIf
671     Mov P_lakkozo2_uj,-150
672     Cnt 0
673     Fine 200
674     Mvs P_lakkozo2_uj
675     Fine 0
676     HClose 1
677     HClose 2
678     Dly 0.1
679     mcode=0
680     mfinish=1
681 '
682 Return
683 '
684 '******************************************************************************
685 *cmoveautolakk1be
686 '
687     Ovrd seb
688     Cnt 1
689     If Dist(P_Curr,P_home)>0.1 Then
690     Mov P_Curr,-150
691     EndIf
692     Mov Plakk_kozott_uj
693     'Mov Plakkozok_kozott
694     Mov P_lakkozo1_uj,-150
695     Cnt 0
696     Fine 200
697     Mvs P_lakkozo1_uj
698     Fine 0
699     mcode=0
700     mfinish=1
701 '
702 Return
703 '
704 '******************************************************************************
705 *cmoveautolakk1ki
706 '
707     Ovrd seb
708     Cnt 1
709     If Dist(P_Curr,P_home)>0.1 Then
710     Mov P_Curr,-150
711     EndIf
712     Mov P_lakkozo1_uj,-150
713     Cnt 0
714     Fine 200
715     Mvs P_lakkozo1_uj
716     Fine 0
717     HClose 1
718     HClose 2
719     Dly 0.1
720     mcode=0
721     mfinish=1
722 '
723 Return
724 '
725 '******************************************************************************
726 *cmoveautoszalag
727 '
728     Ovrd seb
729     Cnt 1
730     If Dist(P_Curr,P_home)>0.1 Then
731         If Dist(P_Curr,Plakkozo2_ki_uj)<0.1 Then
732             Mov Plakk2_ki_elott
733         Else
734             Mov P_Curr,-150
735         EndIf
736     EndIf
737     Mov Psz_felett_uj
738     'Mov Pszalag_felett
739     Cnt 0
740  Ovrd (seb/2)
741     Fine 200
742     Mov Pszalag_uj
743     'Mov Pszalag 'PLACE     'Pszalag ****Paletta mód amíg nincs a helyén az asztal****
744     Fine 0
745     HOpen 2
746     HOpen 1
747  Wait M_In(900) = 1
748     Dly 0.1
749     mcode=0
750     mfinish=1
751 '
752 Return
753 '
754 '******************************************************************************
755 *cmoveautohaj2ki2
756 '
757     Ovrd seb
758     Cnt 1
759     If Dist(P_Curr,P_home)>0.1 Then
760     Mov P_Curr,-150
761     EndIf
762     Mov Phajl2_2_vez1,-150
763     Cnt 0
764     Fine 200
765     Mov Phajl2_2_vez1
766     HClose 1
767     Wait M_In(900) = 0
768     Dly 0.2
769     'Mov Phajl2_2_vez2
770     Ovrd (seb/5)
771     Mvs Phajl2kivesz
772     HClose 2
773     Dly 0.1
774     Fine 0
775     mcode=0
776     mfinish=1
777 '
778 Return
779 '
780 '******************************************************************************
781 *cmoveautohaj1ki2
782 '
783     Ovrd seb
784     Cnt 1
785     If Dist(P_Curr,P_home)>0.1 Then
786     Mov P_Curr,-150
787     EndIf
788     Mov Phajl1_2_vez1,-150
789     Cnt 0
790     Fine 200
791     Mov Phajl1_2_vez1
792     HClose 1
793  Wait M_In(900) = 0
794     Dly 0.2
795     'Mov Phajl1_2_vez2
796  Ovrd (seb/5)
797     Mov Phajl1kivesz
798     HClose 2
799     Dly 0.1
800     Fine 0
801     mcode=0
802     mfinish=1
803 '
804 Return
805 '
806 '******************************************************************************
807 *cmoveautohaj2ki4
808 '
809     Ovrd seb
810     Cnt 1
811     If Dist(P_Curr,P_home)>0.1 Then
812     Mov P_Curr,-150
813     EndIf
814     Mov Phajl2_2_vez2,-150
815     Cnt 0
816     Fine 200
817     Mov Phajl2_2_vez2
818     HClose 1
819     Dly 0.1
820     Mov Phajl2kivesz
821     HClose 2
822     Dly 0.1
823  Fine 0
824     mcode=0
825     mfinish=1
826 '
827 Return
828 '
829 '******************************************************************************
830 *cmoveautohaj1ki4
831 '
832     Ovrd seb
833     Cnt 1
834     If Dist(P_Curr,P_home)>0.1 Then
835     Mov P_Curr,-150
836     EndIf
837     Mov Phajl1_2_vez2,-150
838     Cnt 0
839     Fine 200
840     Mov Phajl1_2_vez2
841     HClose 1
842     Dly 0.1
843     Mov Phajl1kivesz
844     HClose 2
845     Dly 0.1
846     Fine 0
847     mcode=0
848     mfinish=1
849 '
850 Return
851 '
852 '******************************************************************************
853 *cmovefromstation
854 '
855     Ovrd seb
856     Mov P_Curr,-150
857     mcode=0
858     mfinish=1
859 '
860 Return
861 '
862 '******************************************************************************
863 *findnearest                                    ' megkeresi a robot karhoz a legközelebbi pontot a route táblában
864 mnpi = 1
865 mdis = Dist(P_Curr,prout(1))
866 For mi=2 To mrouter
867            m2 = Dist(P_Curr,prout(mi))          'távolság a rout táblában lévõ pontoktól
868            If (m2<mdis) Then
869                       mdis = m2                 'legközelebbi ponttól való távolság
870                       mnpi = mi                 'legközelebbi pont indexe
871            EndIf
872 Next mi
873 '
874 Return
875 '
876 '******************************************************************************
877 *initwhere             'root táblázat feltöltése hazaúthoz
878 If mrstat=1 Then Return
879 GoSub *initrouter                                        'root tábla törlése
880 mrouter = 15
881 '----------------------------------------------------------------------------------------------------------------------------
882 prout(1) = PHome                                        'pontok beállítása
883 prout(2) = Pfeszek_felett
884 prout(3) = Pfeszek_felett
885 prout(4) = P_hajl1_elott
886 prout(5) = P_hajl1_elott
887 prout(6) = P_hajl2_elott
888 prout(7) = P_hajl2_elott
889 prout(8) = Plakk1_elott_uj
890 prout(9) = Plakk1_elott_uj
891 prout(10) = P_lakkozo2_elott
892 prout(11) = P_lakkozo2_elott
893 'prout(12) = Pszalag,
894 prout(12) = Pszalag_uj * (-50.00,+0.00,-100.00)
895 'prout(12) = Pszalag * (-50.00,+0.00,-100.00)
896 prout(13) = Psz_felett_uj
897 'prout(13) = Pszalag_felett
898 prout(14) = Ptmp_home
899 prout(15) = Pszalag_uj
900 'prout(15) = Pszalag
901 '-----------------------------------------------------------------------------------------------------------------------------
902 mrout(1,1) = 0                                    'rout tábla feltöltése hazaúthoz
903 mrout(2,1) = 3
904 mrout(3,1) = 1
905 mrout(4,1) = 5
906 mrout(5,1) = 1
907 mrout(6,1) = 7
908 mrout(7,1) = 1
909 mrout(8,1) = 9
910 mrout(9,1) = 14
911 mrout(10,1) = 11
912 mrout(11,1) = 1
913 mrout(12,1) = 13
914 mrout(13,1) = 14
915 mrout(14,1) = 1
916 mrout(15,1) = 12
917 '-----------------------------------------------------------------------------------------------------------------------------
918 mrstat=1
919 '
920 Return
921 '
922 '******************************************************************************
923 *initrouter                                                  'router táblát törli
924 mrouter = 1                                                'táblázat méret
925 For m1 = 1 To 15
926 For m2 = 1 To 15
927            mrout(m1,m2) = -1                       'tábla feltöltés: -1
928            If (m1=m2) Then mrout(m1,m2)=0          'átló feltöltés:  0
929 Next m2
930 Next m1
931 '
932 Return
933 '
934 '******************************************************************************
935 *route                                                        'route algoritmus
936  Cnt 1, 50, 50
937 *ghloop:                                                     'lépkedés ponról pontra a root táblában
938               mnext = mrout(mnpi,mdsti)            'következõ pont indexe
939               If  mnext=0 Then
940                        'alaphelyzetbe értünk
941               Else
942                        'még van
943               EndIf
944               If (mnext > 0) Then
945                      Mov prout(mnext) Type 1,0     'következõ pontba megy
946                      mnpi = mnext                        'pontindex átadása
947               EndIf
948 If (mnext > 0) Then GoTo *ghloop
949 Cnt 0
950 mst = mnext
951 '
952 Return
953 '
954 ''******************************************************************************
prout(1)=(+1.27,-251.85,+451.89,+178.98,-90.00,-88.98,+0.00,+0.00)(6,0)
prout(2)=(-29.63,-545.72,+368.18,-178.98,-1.22,+0.36,+0.00,+0.00)(7,0)
prout(3)=(-29.63,-545.72,+368.18,-178.98,-1.22,+0.36,+0.00,+0.00)(7,0)
prout(4)=(+278.76,-114.65,+455.83,+46.98,-86.24,+109.99,+0.00,+0.00)(6,0)
prout(5)=(+278.76,-114.65,+455.83,+46.98,-86.24,+109.99,+0.00,+0.00)(6,0)
prout(6)=(+132.64,-209.48,+462.04,+28.25,-87.85,+92.61,+0.00,+0.00)(6,0)
prout(7)=(+132.64,-209.48,+462.04,+28.25,-87.85,+92.61,+0.00,+0.00)(6,0)
prout(8)=(+230.50,+274.28,+581.09,+177.72,+80.84,-136.71,+0.00,+0.00)(6,0)
prout(9)=(+230.50,+274.28,+581.09,+177.72,+80.84,-136.71,+0.00,+0.00)(6,0)
prout(10)=(+388.83,+141.32,+561.07,-103.77,+36.76,-109.64,+0.00,+0.00)(6,1048576)
prout(11)=(+388.83,+141.32,+561.07,-103.77,+36.76,-109.64,+0.00,+0.00)(6,1048576)
prout(12)=(-55.36,+434.60,+563.46,+179.52,+2.57,-89.10,+0.00,+0.00)(7,0)
prout(13)=(-53.26,+309.75,+690.90,+176.50,+19.57,-90.60,+0.00,+0.00)(7,0)
prout(14)=(+321.77,-49.22,+488.13,+43.27,-85.79,+128.35,+0.00,+0.00)(6,0)
prout(15)=(-55.48,+389.13,+461.32,+179.52,+2.57,-89.10,+0.00,+0.00)(7,0)
PHome=(+1.27,-251.85,+451.89,+178.98,-90.00,-88.98)(6,0)
P_service1=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
Pservice1=(+377.31,+169.50,+629.38,+123.36,-90.00,+80.66,+0.00,+0.00)(6,0)
P_service2=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
Pservice2=(+377.31,+169.50,+629.38,+123.36,-90.00,+80.66,+0.00,+0.00)(6,0)
P_home=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
P_feszek=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
Pfeszek=(-29.63,-545.72,+301.98,-178.98,-1.22,+0.36)(7,0)
P_hajl1berak=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
Phajl1berak=(+418.00,-174.30,+491.00,-0.78,-84.52,+158.07)(6,1048576)
P_hajl1kivesz=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
Phajl1kivesz=(+416.73,-173.12,+462.54,+46.98,-86.24,+109.99)(6,0)
P_hajl2berak=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
Phajl2berak=(+215.00,-344.00,+481.50,+11.66,-77.62,+109.45)(6,0)
P_hajl2kivesz=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
Phajl2kivesz=(+209.58,-338.15,+467.00,+28.25,-87.85,+92.61)(6,0)
P_lakkozo1=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
Plakkozo1=(+302.12,+434.37,+524.24,-101.40,+39.06,-63.18)(6,1048576)
P_lakkozo1_uj=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
Plakkozo1_uj=(+332.60,+385.41,+558.60,-136.77,+76.37,-91.95)(6,0)
P_lakkozo2=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
Plakkozo2=(+533.22,+112.48,+532.47,-103.77,+36.76,-109.64)(6,1048576)
P_lakkozo2_uj=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
Plakkozo2_uj=(+533.22,+112.48,+532.47,-103.66,+36.18,-109.46)(6,0)
P_szalag_uj=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
Pszalag_uj=(-55.48,+389.13,+461.32,+179.52,+2.57,-89.10)(7,0)
P_szfelett_uj=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
Psz_felett_uj=(-53.26,+309.75,+690.90,+176.50,+19.57,-90.60)(7,0)
P_hajl1_elott=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
PSzor=(+0.00,+0.00,-150.00,+0.00,+0.00,+0.00,+0.00,+0.00)(6,0)
P_hajl2_elott=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
P_lakkozo1_elott=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
P_lakkozo2_elott=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
PEscape=(-11.31,-50.00,-8.06,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PDestination=(+371.22,-67.94,+535.88,+98.58,+82.02,+63.58,+0.00,+0.00)(6,0)
ptmp=(+230.50,+274.28,+581.09,-136.77,+76.37,-91.95,+0.00,+0.00)(6,0)
Pfeszek_felett=(-29.63,-545.72,+368.18,-178.98,-1.22,+0.36)(7,0)
Plakkozo1_elott=(+158.76,+325.14,+548.35,-99.67,+39.92,-60.39)(6,1048576)
Plakkozo2_elott=(+537.06,+117.97,+536.78,-106.37,+35.81,-114.04)(6,0)
Pszalag=(-66.60,+390.87,+454.40,-179.97,+3.68,-94.73)(6,1048576)
Pszalag_felett=(-37.39,+296.00,+777.04,-179.96,+34.65,-94.70)(6,1048576)
Phajl2_elott=(+157.12,-256.81,+549.46,+173.43,-77.12,-50.54)(6,1048576)
Phajl1_elott=(+288.70,-117.52,+546.03,-178.84,-79.66,-23.49)(6,0)
Plakkozok_kozott=(+237.98,-185.15,+508.35,-84.61,+40.79,-136.87)(6,1048576)
Plakk_kozott_uj=(+314.19,-27.60,+598.82,+90.86,-1.48,+85.05)(6,0)
Plakkozo2_ki_uj=(+533.22,+112.48,+532.47,-103.65,+36.09,-109.44)(6,0)
Plakk2_ki_elott=(+321.91,+106.66,+561.27,-90.01,+41.93,-90.01)(6,0)
Phajl2_2_vez1=(+201.56,-324.08,+513.71,+2.33,-64.27,+118.65)(6,0)
Phajl1_2_vez1=(+403.11,-166.09,+501.05,+8.32,-70.29,+148.92)(6,0)
Phajl2_2_vez2=(+204.15,-330.73,+450.00,+4.35,-76.49,+116.52)(6,0)
Phajl1_2_vez2=(+416.57,-173.30,+460.98,+41.34,-85.75,+115.51)(6,0)
Plakk1_elott_uj=(+230.50,+274.28,+581.09,+177.72,+80.84,-136.71)(6,0)
Ptmp_home=(+321.77,-49.22,+488.13,+43.27,-85.79,+128.35,+0.00,+0.00)(6,0)
P1=(+169.00,+570.00,+135.00,+178.64,+0.05,-89.05)(7,0)
P2=(-248.27,+570.00,+135.00,+178.64,+0.05,-89.05)(7,0)
P3=(+169.00,+570.00,+148.00,+178.64,+0.05,-89.05)(7,0)
P4=(+84.77,+528.21,+148.12,+178.58,-0.15,-91.73)(6,0)
Phajl1_elott2=(+349.63,-142.52,+534.02,-179.18,-75.27,-23.14)(6,0)
Phajl2_elott2=(+136.98,-209.88,+464.31,-87.79,+19.96,-148.01)(6,1048576)
Phome2=(+0.43,-297.45,+453.24,+30.71,-88.08,+58.20)(6,0)
PLACE=(+169.00,+570.00,+135.00,+178.64,+0.05,-89.05,+0.00,+0.00)(7,0)
Plakkozo1_backup=(+301.29,+436.48,+526.08,-103.00,+38.63,-65.73)(6,1048576)
Plakkozo2_backup=(+537.06,+117.97,+536.78,-106.37,+35.81,-114.04)(6,1048576)
Plakkozo1_u=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
Jmargit=(-89.56,-22.11,+153.77,+0.66,-41.66,+179.51)
