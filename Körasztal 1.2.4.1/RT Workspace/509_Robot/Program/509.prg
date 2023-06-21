1 '***************************************************************************
2 '*************************      509 Körasztal      *************************
3 '***************************************************************************
4 'PDestination és PEscape - ütközáskor használjuk
5 'Pfészek - megfogási pont
6 'Phajl1_elott - számolt poz (Pszor)
7 'Phajl1berak - Mindegyik terméknél egyforma
8 'Phajl1_2_vez1 és 2 - alsó megfogó megfog és lehúz
9 'Phajl1kivesz - utolsó pont, ahol a felsõ pofa megfog
10 'Plakkozó_elott - számolt poz (PSzor)
11 'PSzor - eltolás
12 'P_xxx - nem felvett pontok, értéket kapnak
13 '
14 '***************************************************************************
15 'DEKLARÁCIÓ
16 '***************************************************************************
17 Dim mpa(10)               'paraméter tömb
18 Dim mres(9)               'eredmény tömb
19 Dim prout(15)             'pontlista
20 Dim mrout(15,15)          'rout tábla
21 '
22 '***************************************************************************
23 'ALAPHELYZET
24 '***************************************************************************
25 Accel 70,70
26 Spd 100000&
27 Ovrd 5
28 ColLvl 100,100,100,150,100,100,,
29 HOpen 1
30 HOpen 2
31 For mi=0 To 12
32 M_Outw(10160+mi*16)=0       'Kimenetek inicializálása
33 Next mi
34 mcmd_cnt=0                              'parancs számláló
35 mcmd_num=0                            'parancs szám
36 mcmd=0                                    'parancs
37 mrouter=1                                 'root táblázat méret
38 mrstat=0                                   'root táblát inicializálni kell/nemkell
39 mdis=1                                      'távolság a legközelebbi ponttól
40 '
41 '***************************************************************************
42 'ÜTKÖZÉSVÉDELEM
43 '***************************************************************************
44 Def Act 5,M_ColSts(1)=1 GoTo *deepimpact,S
45 Act 5=1
46 ColChk On, NOErr
47 '
48 '***************************************************************************
49 'Vészleállás
50 '***************************************************************************
51 EStop=M_Inw(10544)
52 Def Act 2,M_Inw(10544)>0 GoTo *emergencystop,S
53 Act 2=1
54 '
55 '***************************************************************************
56 'FÖPROGRAM CIKLUS
57 '***************************************************************************
58 '
59 While 1
60 '***************************************************************************
61 'Pozíció korrekciók
62 '***************************************************************************
63 Phajl1berak_k = Phajl1berak
64 Phajl1berak_k.Z = Phajl1berak.Z +(M_Inw(10560)/1000)
65 Phajl2berak_k = Phajl2berak
66 Phajl2berak_k.Z = Phajl2berak.Z +(M_Inw(10576)/1000)
67 Plakkozo1_k = Plakkozo1
68 Plakkozo1_k.Z = Plakkozo1.Z +(M_Inw(10592)/1000)
69 Plakkozo2_k = Plakkozo2
70 Plakkozo2_k.Z = Plakkozo2.Z +(M_Inw(10608)/1000)
71 Plakkozo2ki_k = Plakkozo2ki
72 Plakkozo2ki_k.Z = Plakkozo2ki.Z +(M_Inw(10608)/1000)
73 Phajl1_elott = Phajl1kivesz * PSzor
74 Phajl2_elott = Phajl2kivesz * PSzor
75 Plakkozo2_elott = Plakkozo2 * PSzor
76 '
77 '***************************************************************************
78 'Globális pozíciók
79 '***************************************************************************
80 P_home = PHome
81 P_feszek = Pfeszek
82 P_hajl1berak = Phajl1berak_k
83 P_hajl1kivesz = Phajl1kivesz
84 P_hajl2berak =  Phajl2berak_k
85 P_hajl2kivesz = Phajl2kivesz
86 P_lakkozo1 = Plakkozo1_k
87 P_lakkozo2 = Plakkozo2_k
88 P_lakkozo2ki = Plakkozo2ki_k
89 P_szalag = Pszalag
90 P_szalag_felett = Pszalag_felett
91 P_service1 = Pservice1
92 P_service2 = Pservice2
93 '
94 '***************************************************************************
95 'Értékadások
96 '***************************************************************************
97 mcmd_num=M_In32(10160)          'parancsszám beolvasása
98 seb=M_Inw(10480)               'Sebesség a plc-tõl
99 '***************************************************************************
100 '
101 If mcmd_num <> 0 Then
102 '
103      mcode = -1                                                      'visszatérési érték -1
104 '
105     If mcmd_num > mcmd_cnt Then
106         For mi=1 To 10
107             mpa(mi)=0                                               'paraméterek törlése
108         Next mi
109     '
110         For mi=1 To 9
111             mres(mi)=0                                              'eredmények törlése
112         Next mi
113     '
114         mfinish=0                                                     'subrutin lefutott jel nullázása
115         mcmd_cnt=mcmd_num                                 'parancs számláló növelése
116         mcmd=M_Inw(10192)                                   'parancs beolvasása
117         For mi=1 To 10
118             mpa(mi)=M_Inw(10192+mi*16)                 'paraméterek beolvasása
119         Next mi
120     '
121         mstate=1
122         GoSub *statemachine                                    'ugrás az állapotgépre
123     Else
124         GoSub *statemachine
125     EndIf
126     '
127     If mfinish=1 Then
128          For mi=1 To 9
129              M_Outw(10208+mi*16)=mres(mi)            'eredmények visszaküldése
130          Next mi
131          M_Outw(10208)=mcode                               'hibakód visszaküldése
132          M_Outw(10192)=mcmd                                'parancs visszaküldése
133          M_Out32(10160)=mcmd_num                        'parancsszám visszaküldése
134          Dly 0.1
135          mfinish=0
136          mcmd=0
137          mstate=0
138     EndIf
139 '
140 Else
141 '
142      mcmd_cnt=0                                                    'ha parancsszám 0 -> parancs számláló 0
143 '
144 EndIf
145 '
146 WEnd
147 '
148 '***************************************************************************
149 'Állapotgép
150 '***************************************************************************
151 *statemachine
152 '
153 Select mcmd                                                'parancs végrehajtás, szubrutin hívások
154 Case 0
155              Dly 0.05
156              Break
157 Case 2
158              GoSub *chopen                                  'megfogó nyit
159              Break
160 Case 3
161              GoSub *chclose                                 'megfogó zár
162              Break
163 Case 6
164              GoSub *cgohome                                 'alaphelyzetbe áll
165              Break
166 Case 7
167              GoSub *cgoservice                                 'szervíz helyzetbe áll
168              Break
169 Case 20
170              GoSub *cmoveautofeszek
171              Break
172 Case 21
173              GoSub *cmoveautohajl2be
174              Break
175 Case 22
176              GoSub *cmoveautohajl2ki
177              Break
178 Case 23
179              GoSub *cmoveautohajl1be
180              Break
181 Case 24
182              GoSub *cmoveautohajl1ki
183              Break
184 Case 25
185              GoSub *cmoveautolakk2be
186              Break
187 Case 26
188              GoSub *cmoveautolakk2ki
189              Break
190 Case 27
191              GoSub *cmoveautolakk1be
192              Break
193 Case 28
194              GoSub *cmoveautolakk1ki
195              Break
196 Case 30
197              GoSub *cmoveautoszalag
198              Break
199 Case 31
200              GoSub  *cmoveautohaj2ki2
201              Break
202 Case 32
203              GoSub  *cmoveautohaj1ki2
204              Break
205 Case 33
206              GoSub  *cmoveautohaj2ki4
207              Break
208 Case 34
209              GoSub  *cmoveautohaj1ki4
210              Break
211 Case 40
212              GoSub *cmovefromstation
213              Break
214 Case 41
215              GoSub *cmove2position                          'mozgás célkoordinátákhoz
216              Break
217 Case 51                                                     'abort parancs
218              GoSub *cabort
219              Break
220 Default
221              mfinish=1
222              Break
223 End Select
224 Return
225 '
226 '***************************************************************************
227 'ÜTKÖZÉS
228 '***************************************************************************
229 *deepimpact
230 '
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
241 '
242 '***************************************************************************
243 'E Stop
244 '***************************************************************************
245 *emergencystop
246 '
247 Servo Off
248 Error 9100
249 '
250 '***************************************************************************
251 'MEGFOGO NYIT
252 '***************************************************************************
253 *chopen
254 '
255 Select mstate
256 Case 1
257               If ((mpa(1) >=1) And (mpa(1)<=2)) Then
258                        mstate=mstate+1
259               Else
260                        mcode= -2
261                        mfinish=1
262               EndIf
263               Break
264 Case 2
265                HOpen mpa(1)
266                mstate=mstate+1
267                Break
268 Case 3
269                If M_In(900+(mpa(1)-1)) = 1 Then
270                       Dly 0.1
271                       mstate=mstate+1
272                EndIf
273                Break
274 Case 4
275                mres(1)=mpa(1)
276                mcode=0
277                mfinish=1
278                Break
279 End Select
280 '
281 Return
282 '
283 '***************************************************************************
284 'MEGFOGO ZÁR
285 '***************************************************************************
286 *chclose
287 '
288 Select mstate
289 Case 1
290               If ((mpa(1)>=1) And (mpa(1)<=2)) Then
291                        mstate=mstate+1
292               Else
293                        mcode= -2
294                        mfinish=1
295               EndIf
296               Break
297 Case 2
298                HClose mpa(1)
299                mstate=mstate+1
300                Break
301 Case 3
302                If M_In(900+(mpa(1)-1)) = 0 Then
303                    Dly 0.1
304                    mstate=mstate+1
305                EndIf
306                Break
307 Case 4
308                mres(1)=mpa(1)
309                mcode=0
310                mfinish=1
311                Break
312 End Select
313 '
314 Return
315 '
316 '******************************************************************************
317 'HOME
318 '******************************************************************************
319 *cgohome
320 '
321 Select mstate
322 Case 1
323               If ((mpa(1)>=1) And (mpa(1)<=2)) Then
324                        mstate=mstate+1
325               Else
326                        mcode= -2
327                        mfinish=1
328               EndIf
329               Break
330 Case 2
331                HOpen 1
332                HOpen 2
333                If (M_In(900) = 1 And M_In(901) = 1) Then
334                    mstate=4
335                EndIf
336                Break
337 Case 3
338                'HOpen 2
339                'If M_In(901) = 1 Then
340                    mstate=mstate+1
341                'EndIf
342                'Break
343 Case 4
344                If Dist(P_Curr,PHome)<5 Then
345                    Mov PHome
346                    mstate=8
347                Else
348                    mstate=mstate+1
349                EndIf
350                Break
351 Case 5
352                If mpa(1)=1 Then Ovrd seb
353                If mpa(1)=2 Then Ovrd seb
354                mstate=mstate+1
355                Break
356 Case 6
357                GoSub *initwhere
358                GoSub *findnearest                               'legközelebbi pont megkeresése
359                If mdis>10 Then                                  'ha a távolság nagyobb mint 10mm
360                    ptmp = P_Curr
361                    ptmp.X = prout(mnpi).X
362                    ptmp.Y = prout(mnpi).Y
363                    ptmp.Z = prout(mnpi).Z
364                    Mvs ptmp
365                    Mov prout(mnpi)                               'mozgás a legközelebbi pontba
366                EndIf
367                mstate=mstate+1
368                Break
369 Case 7
370                mdsti = 1                                          'célpont index
371                GoSub *route                                       'pontról pontra lépkedés a root táblában
372                mstate=mstate+1
373                Break
374 Case 8
375                mcode=0
376                mfinish=1
377                Break
378 End Select
379 '
380 Return
381 '
382 '******************************************************************************
383 'SZERVÍZ POZÍCIÓ
384 '******************************************************************************
385 *cgoservice
386 Select mstate
387 Case 1
388                 Select mpa(1)
389                     Case 1
390                         Ovrd 10
391                         Cnt 1
392                         Mov Pservice1
393                         Mov Pservice1, 100
394                         Cnt 0
395                     Case 2
396                         Ovrd 10
397                         Cnt 1
398                         Mov Pservice2
399                         Mov Pservice2, 100
400                         Cnt 0
401                 End Select
402                 mres(1)=mpa(1)
403                 mcode=0
404                 mfinish=1
405 End Select
406 '
407 Return
408 '
409 '******************************************************************************
410 'MOZGÁS TESZT
411 '******************************************************************************
412 *cmove2position
413 '
414 Select mstate
415 Case 1
416                  If (mpa(1)>=1) And (mpa(1)<=13) Then
417                     mstate=mstate+1
418                  Else
419                     mcode= -2
420                     mfinish=1
421                  EndIf
422                  Break
423 Case 2
424                  Select mpa(1)
425                     Case 1
426                         mstate=3
427                     Case 2
428                         mstate=4
429                     Case 3
430                         mstate=5
431                     Case 4
432                         mstate=6
433                     Case 5
434                         mstate=7
435                     Case 6
436                         mstate=8
437                     Case 7
438                         mstate=9
439                     Case 8
440                         mstate=10
441                     Case 9
442                         mstate=11
443                     Case 10
444                         mstate=12
445                     Case 11
446                         mstate=13
447                     Case 12
448                         mstate=14
449                     Case 13
450                         mstate=15
451                  End Select
452                  Break
453 Case 3
454                  Ovrd seb
455                  Fine 100
456                  Mov PHome
457                  mstate=16
458                  Break
459 Case 4
460                  Ovrd seb
461                  Mov Pfeszek,-150
462                  Fine 100
463                  Mov Pfeszek
464                  mstate=16
465                  Break
466 Case 5
467                  Ovrd seb
468                  Mov Pfeszek_felett,-170
469                  Fine 100
470                  Mov Pfeszek_felett
471                  mstate=16
472                  Break
473 Case 6
474                  Ovrd seb
475                  Mov P_hajl1berak,-150
476                  Fine 100
477                  Mvs P_hajl1berak
478                  mstate=16
479                  Break
480 Case 7
481                  Ovrd seb
482                  Mov Phajl1kivesz,-150
483                  Fine 100
484                  Mov Phajl1kivesz
485                  mstate=16
486                  Break
487 Case 8
488                  Ovrd seb
489                  Mov P_hajl2berak,-150
490                  Fine 100
491                  Mvs P_hajl2berak
492                  mstate=16
493                  Break
494 Case 9
495                  Ovrd seb
496                  Mov Phajl2kivesz,-150
497                  Fine 100
498                  Mov Phajl2kivesz
499                  mstate=16
500                  Break
501 Case 10
502                  Ovrd seb
503                  Fine 100
504                  Mov P_lakkozo1,-150
505                  Fine 100
506                  Mvs P_lakkozo1
507                  mstate=16
508                  Break
509 Case 11
510                  Ovrd seb
511                  Fine 100
512                  Mov P_lakkozo1,-150
513                  mstate=16
514                  Break
515 Case 12
516                  Ovrd seb
517                  Fine 100
518                  Mov P_lakkozo2,-150
519                  Mvs P_lakkozo2
520                  mstate=16
521                  Break
522 Case 13
523                  Ovrd seb
524                  Fine 100
525                  Mov Plakkozo2_elott
526                  mstate=16
527                  Break
528 Case 14
529                  Ovrd seb
530                  Fine 100
531                  Mov Pszalag
532                  mstate=16
533                  Break
534 Case 15
535                  Ovrd seb
536                  Fine 100
537                  Mov Pszalag_felett
538                  mstate=16
539                  Break
540 Case 16
541                  mres(1)=mpa(1)
542                  mcode=0
543                  mfinish=1
544                  Break
545 End Select
546 '
547 Return
548 '
549 '******************************************************************************
550 *cabort
551 '
552 mstate=0
553 mcode=0
554 mfinish=1
555 '
556 Return
557 '
558 '******************************************************************************
559 *cmoveautofeszek
560 '
561     Ovrd seb
562     Mov Pfeszek,-20
563     Fine 100
564     Mov Pfeszek
565     HClose 2
566     Dly 0.1
567     Fine 0
568     mcode=0
569     mfinish=1
570 '
571 Return
572 '
573 '******************************************************************************
574 *cmoveautohajl2be
575 '
576     Ovrd seb
577     Cnt 1
578     Mov Pfeszek,-150
579     Mov PHome
580     Mvs Phajl2_elott*(+0.00,+4.00,+0.00)
581     Cnt 0
582     Fine 100
583     Mvs Phajl2berak_k*(+0.00,+4.00,+0.00)
584     Fine 0
585     mcode=0
586     mfinish=1
587 '
588 Return
589 '
590 '******************************************************************************
591 *cmoveautohajl2ki
592 '
593     Ovrd seb
594     Cnt 1
595     If Dist(P_Curr,PHome)>0.1 Then
596     Mov P_Curr,-150
597     EndIf
598     Mov Phajl2kivesz,-150
599     Cnt 0
600     Fine 100
601     Mov Phajl2kivesz
602     Fine 0
603     HClose 2
604     Dly 0.1
605     mcode=0
606     mfinish=1
607 '
608 Return
609 '
610 '******************************************************************************
611 *cmoveautohajl1be
612 '
613     Ovrd seb
614     Cnt 1
615     Mov Pfeszek,-150
616     Mov PHome
617     Ovrd (seb/1.5)
618     Mov Phajl1_elott
619     Cnt 0
620     Fine 200
621     Mvs Phajl1berak_k
622     Fine 0
623     Ovrd seb
624     mcode=0
625     mfinish=1
626 '
627 Return
628 '
629 '******************************************************************************
630 *cmoveautohajl1ki
631 '
632     Ovrd seb
633     Cnt 1
634     If Dist(P_Curr,PHome)>0.1 Then
635     Mov P_Curr,-150
636     EndIf
637     Mov Phajl1kivesz,-150
638     Cnt 0
639     Fine 100
640     Mov Phajl1kivesz
641     Fine 0
642     HClose 2
643     Dly 0.1
644     mcode=0
645     mfinish=1
646 '
647 Return
648 '
649 '******************************************************************************
650 *cmoveautolakk2be
651 '
652     Ovrd seb
653     Cnt 1
654     If Dist(P_Curr,PHome)>0.1 Then
655     Mov P_Curr,-150
656     EndIf
657     Mov Plakk_kozott_uj
658     Mov Plakkozo2_k, -150
659     Cnt 0
660     Fine 200
661     Mvs Plakkozo2_k
662     Fine 0
663     mcode=0
664     mfinish=1
665 '
666 Return
667 '
668 '******************************************************************************
669 *cmoveautolakk2ki
670 '
671     Ovrd seb
672     Cnt 1
673     If Dist(P_Curr,PHome)>0.1 Then
674     Mov P_Curr,-150
675     EndIf
676     Mov Plakkozo2_k,-150
677     Cnt 0
678     Fine 200
679     Mvs Plakkozo2_k
680     Fine 0
681     HClose 1
682     HClose 2
683     Dly 0.1
684     mcode=0
685     mfinish=1
686 '
687 Return
688 '
689 '******************************************************************************
690 *cmoveautolakk1be
691 '
692     Ovrd seb
693     Cnt 1
694     If Dist(P_Curr,PHome)>0.1 Then
695     Mov P_Curr,-150
696     EndIf
697     Mov Plakk_kozott_uj
698     Mov Plakkozo1_k,-150
699     Cnt 0
700     Fine 200
701     Mvs Plakkozo1_k
702     Fine 0
703     mcode=0
704     mfinish=1
705 '
706 Return
707 '
708 '******************************************************************************
709 *cmoveautolakk1ki
710 '
711     Ovrd seb
712     Cnt 1
713     If Dist(P_Curr,PHome)>0.1 Then
714     Mov P_Curr,-150
715     EndIf
716     Mov Plakkozo1_k,-150
717     Cnt 0
718     Fine 200
719     Mvs Plakkozo1_k
720     Fine 0
721     HClose 1
722     HClose 2
723     Dly 0.1
724     mcode=0
725     mfinish=1
726 '
727 Return
728 '
729 '******************************************************************************
730 *cmoveautoszalag
731 '
732     Ovrd seb
733     Cnt 1
734     If Dist(P_Curr,PHome)>0.1 Then
735         'If Dist(P_Curr,Plakkozo2_k)<0.1 Then
736         '    Mov Plakk2_ki_elott
737         'Else
738             Mov P_Curr,-150
739         'EndIf
740     EndIf
741     Mov Pszalag_felett
742     Cnt 0
743     Ovrd (seb/2)
744     Fine 200
745     Mov Pszalag
746     Fine 0
747     HOpen 2
748     HOpen 1
749     Wait M_In(900) = 1
750     Dly 0.1
751     mcode=0
752     mfinish=1
753 '
754 Return
755 '
756 '******************************************************************************
757 *cmoveautohaj2ki2
758 '
759     Ovrd seb
760     Cnt 1
761     If Dist(P_Curr,PHome)>0.1 Then
762     Mov P_Curr,-150
763     EndIf
764     Mov Phajl2_2_vez1,-150
765     Cnt 0
766     Fine 200
767     Mov Phajl2_2_vez1
768     HClose 1
769     Wait M_In(900) = 0
770     Dly 0.2
771     Ovrd (seb/5)
772     Mvs Phajl2kivesz
773     HClose 2
774     Dly 0.1
775     Fine 0
776     mcode=0
777     mfinish=1
778 '
779 Return
780 '
781 '******************************************************************************
782 *cmoveautohaj1ki2
783 '
784     Ovrd seb
785     Cnt 1
786     If Dist(P_Curr,PHome)>0.1 Then
787     Mov P_Curr,-150
788     EndIf
789     Mov Phajl1_2_vez1,-150
790     Cnt 0
791     Fine 200
792     Mov Phajl1_2_vez1
793     HClose 1
794     Wait M_In(900) = 0
795     Dly 0.2
796     Ovrd (seb/5)
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
811     If Dist(P_Curr,PHome)>0.1 Then
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
834     If Dist(P_Curr,PHome)>0.1 Then
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
885 prout(4) = Phajl1_elott
886 prout(5) = Phajl1_elott
887 prout(6) = Phajl2_elott
888 prout(7) = Phajl2_elott
889 prout(8) = Plakkozo1_elott
890 prout(9) = Plakkozo1_elott
891 prout(10) = Plakkozo2_elott
892 prout(11) = Plakkozo2_elott
893 prout(12) = Pszalag * (-50.00,+0.00,-100.00)
894 prout(13) = Pszalag_felett
895 prout(14) = Ptmp_home
896 prout(15) = Pszalag
897 '-----------------------------------------------------------------------------------------------------------------------------
898 mrout(1,1) = 0                                    'rout tábla feltöltése hazaúthoz
899 mrout(2,1) = 3
900 mrout(3,1) = 1
901 mrout(4,1) = 5
902 mrout(5,1) = 1
903 mrout(6,1) = 7
904 mrout(7,1) = 1
905 mrout(8,1) = 9
906 mrout(9,1) = 14
907 mrout(10,1) = 11
908 mrout(11,1) = 1
909 mrout(12,1) = 13
910 mrout(13,1) = 14
911 mrout(14,1) = 1
912 mrout(15,1) = 12
913 '-----------------------------------------------------------------------------------------------------------------------------
914 mrstat=1
915 '
916 Return
917 '
918 '******************************************************************************
919 *initrouter                                                  'router táblát törli
920 mrouter = 1                                                'táblázat méret
921 For m1 = 1 To 15
922 For m2 = 1 To 15
923            mrout(m1,m2) = -1                       'tábla feltöltés: -1
924            If (m1=m2) Then mrout(m1,m2)=0          'átló feltöltés:  0
925 Next m2
926 Next m1
927 '
928 Return
929 '
930 '******************************************************************************
931 *route                                                        'route algoritmus
932  Cnt 1, 50, 50
933 *ghloop:                                                     'lépkedés ponról pontra a root táblában
934               mnext = mrout(mnpi,mdsti)            'következõ pont indexe
935               If  mnext=0 Then
936                        'alaphelyzetbe értünk
937               Else
938                        'még van
939               EndIf
940               If (mnext > 0) Then
941                      Mov prout(mnext) Type 1,0     'következõ pontba megy
942                      mnpi = mnext                        'pontindex átadása
943               EndIf
944 If (mnext > 0) Then GoTo *ghloop
945 Cnt 0
946 mst = mnext
947 '
948 Return
949 '
950 ''******************************************************************************
prout(1)=(+1.27,-251.85,+451.89,+178.98,-90.00,-88.98,+0.00,+0.00)(6,0)
prout(2)=(-29.63,-545.72,+368.18,-178.98,-1.22,+0.36,+0.00,+0.00)(7,0)
prout(3)=(-29.63,-545.72,+368.18,-178.98,-1.22,+0.36,+0.00,+0.00)(7,0)
prout(4)=(+278.76,-114.65,+455.83,+46.98,-86.24,+109.99,+0.00,+0.00)(6,0)
prout(5)=(+278.76,-114.65,+455.83,+46.98,-86.24,+109.99,+0.00,+0.00)(6,0)
prout(6)=(+132.64,-209.48,+462.04,+28.25,-87.85,+92.61,+0.00,+0.00)(6,0)
prout(7)=(+132.64,-209.48,+462.04,+28.25,-87.85,+92.61,+0.00,+0.00)(6,0)
prout(8)=(+230.50,+274.28,+581.09,+177.72,+80.84,-136.71,+0.00,+0.00)(6,0)
prout(9)=(+230.50,+274.28,+581.09,+177.72,+80.84,-136.71,+0.00,+0.00)(6,0)
prout(10)=(+374.66,+49.68,+599.59,-148.42,+75.12,-148.08,+0.00,+0.00)(6,0)
prout(11)=(+374.66,+49.68,+599.59,-148.42,+75.12,-148.08,+0.00,+0.00)(6,0)
prout(12)=(-55.36,+434.60,+563.46,+179.52,+2.57,-89.10,+0.00,+0.00)(7,0)
prout(13)=(-53.26,+309.75,+690.90,+176.50,+19.57,-90.60,+0.00,+0.00)(7,0)
prout(14)=(+321.77,-49.22,+488.13,+43.27,-85.79,+128.35,+0.00,+0.00)(6,0)
prout(15)=(-55.48,+389.13,+461.32,+179.52,+2.57,-89.10,+0.00,+0.00)(7,0)
Phajl1berak_k=(+418.00,-174.30,+491.00,-0.78,-84.52,+158.07)(6,1048576)
Phajl1berak=(+418.00,-174.30,+491.00,-0.78,-84.52,+158.07)(6,1048576)
Phajl2berak_k=(+215.00,-344.00,+481.50,+11.66,-77.62,+109.45)(6,0)
Phajl2berak=(+215.00,-344.00,+481.50,+11.66,-77.62,+109.45)(6,0)
Plakkozo1_k=(+332.60,+385.41,+558.60,-136.77,+76.37,-91.95)(6,0)
Plakkozo1=(+332.60,+385.41,+558.60,-136.77,+76.37,-91.95)(6,0)
Plakkozo2_k=(+521.02,+48.30,+566.78,-148.42,+75.12,-148.08)(6,0)
Plakkozo2=(+521.02,+48.30,+566.78,-148.42,+75.12,-148.08)(6,0)
Plakkozo2ki_k=(+533.22,+112.48,+532.47,-103.66,+36.18,-109.46)(6,0)
Plakkozo2ki=(+533.22,+112.48,+532.47,-103.66,+36.18,-109.46)(6,0)
Phajl1_elott=(+278.76,-114.65,+455.83,+46.98,-86.24,+109.99)(6,0)
Phajl1kivesz=(+416.73,-173.12,+462.54,+46.98,-86.24,+109.99)(6,0)
PSzor=(+0.00,+0.00,-150.00,+0.00,+0.00,+0.00,+0.00,+0.00)(6,0)
Phajl2_elott=(+132.64,-209.48,+462.04,+28.25,-87.85,+92.61)(6,0)
Phajl2kivesz=(+209.58,-338.15,+467.00,+28.25,-87.85,+92.61)(6,0)
Plakkozo2_elott=(+374.66,+49.68,+599.59,-148.42,+75.12,-148.08)(6,0)
P_home=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
PHome=(+1.27,-251.85,+451.89,+178.98,-90.00,-88.98)(6,0)
P_feszek=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
Pfeszek=(-29.63,-545.72,+301.98,-178.98,-1.22,+0.36)(7,0)
P_hajl1berak=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
P_hajl1kivesz=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
P_hajl2berak=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
P_hajl2kivesz=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
P_lakkozo1=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
P_lakkozo2=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
P_lakkozo2ki=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
P_szalag=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
Pszalag=(-55.48,+389.13,+461.32,+179.52,+2.57,-89.10)(7,0)
P_szalag_felett=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
Pszalag_felett=(-53.26,+309.75,+690.90,+176.50,+19.57,-90.60)(7,0)
P_service1=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
Pservice1=(+377.31,+169.50,+629.38,+123.36,-90.00,+80.66,+0.00,+0.00)(6,0)
P_service2=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
Pservice2=(+377.31,+169.50,+629.38,+123.36,-90.00,+80.66,+0.00,+0.00)(6,0)
PEscape=(-50.00,-17.60,-20.08,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PDestination=(+145.36,-218.26,+471.15,-86.00,-11.25,-144.18,+0.00,+0.00)(6,1048576)
ptmp=(+132.64,-209.48,+462.04,+28.25,-87.85,+92.61,+0.00,+0.00)(6,0)
Pfeszek_felett=(-29.63,-545.72,+368.18,-178.98,-1.22,+0.36)(7,0)
Plakk_kozott_uj=(+314.19,-27.60,+598.82,+90.86,-1.48,+85.05)(6,0)
Phajl2_2_vez1=(+201.56,-324.08,+513.71,+2.33,-64.27,+118.65)(6,0)
Phajl1_2_vez1=(+403.11,-166.09,+501.05,+8.32,-70.29,+148.92)(6,0)
Phajl2_2_vez2=(+204.15,-330.73,+450.00,+4.35,-76.49,+116.52)(6,0)
Phajl1_2_vez2=(+416.57,-173.30,+460.98,+41.34,-85.75,+115.51)(6,0)
Plakkozo1_elott=(+230.50,+274.28,+581.09,+177.72,+80.84,-136.71)(6,0)
Ptmp_home=(+321.77,-49.22,+488.13,+43.27,-85.79,+128.35,+0.00,+0.00)(6,0)
Plakk2_ki_elott=(+321.91,+106.66,+561.27,-90.01,+41.93,-90.01)(6,0)
Plakkozo2_=(+517.74,+48.45,+560.56,-139.47,+78.38,-139.08)(6,0)
Plakkozo2_k_=(+533.22,+112.48,+532.47,-103.77,+36.76,-109.64)(6,1048576)
Plakkozok_kozott=(+237.98,-185.15,+508.35,-84.61,+40.79,-136.87)(6,1048576)
Jmargit=(-89.56,-22.11,+153.77,+0.66,-41.66,+179.51)
