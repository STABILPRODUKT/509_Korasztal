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
28 HOpen 1
29 HOpen 2
30 For mi=0 To 11
31 M_Outw(10160+mi*16)=0       'Kimenetek inicializálása
32 Next mi
33 mcmd_cnt=0                              'parancs számláló
34 mcmd_num=0                            'parancs szám
35 mcmd=0                                    'parancs
36 mrouter=1                                 'root táblázat méret
37 mrstat=0                                   'root táblát inicializálni kell/nemkell
38 mdis=1                                      'távolság a legközelebbi ponttól
39 '
40 '***************************************************************************
41 'ÜTKÖZÉSVÉDELEM
42 '***************************************************************************
43 Def Act 5,M_ColSts(1)=1 GoTo *deepimpact,S
44 Act 5=1
45 ColChk On, NOErr
46 '
47 '***************************************************************************
48 'Vészleállás
49 '***************************************************************************
50 EStop=M_Inw(10544)
51 Def Act 2,M_Inw(10544)>0 GoTo *emergencystop,S
52 Act 2=1
53 '
54 '***************************************************************************
55 'FÖPROGRAM CIKLUS
56 '***************************************************************************
57 '
58 While 1
59 '***************************************************************************
60 'Pozíció korrekciók
61 '***************************************************************************
62 Phajl1berak_k = Phajl1berak
63 Phajl1berak_k.Z = Phajl1berak.Z +(M_Inw(10560)/1000)
64 Phajl2berak_k = Phajl2berak
65 Phajl2berak_k.Z = Phajl2berak.Z +(M_Inw(10576)/1000)
66 Plakkozo1_k = Plakkozo1
67 Plakkozo1_k.Z = Plakkozo1.Z +(M_Inw(10592)/1000)
68 Plakkozo2_k = Plakkozo2
69 Plakkozo2_k.Z = Plakkozo2.Z +(M_Inw(10608)/1000)
70 Plakkozo2ki_k = Plakkozo2ki
71 Plakkozo2ki_k.Z = Plakkozo2ki.Z +(M_Inw(10608)/1000)
72 Phajl1_elott = Phajl1kivesz * PSzor
73 Phajl2_elott = Phajl2kivesz * PSzor
74 Plakkozo2_elott = Plakkozo2 * PSzor
75 '
76 '***************************************************************************
77 'Globális pozíciók
78 '***************************************************************************
79 P_home = PHome
80 P_feszek = Pfeszek
81 P_hajl1berak = Phajl1berak_k
82 P_hajl1kivesz = Phajl1kivesz
83 P_hajl2berak =  Phajl2berak_k
84 P_hajl2kivesz = Phajl2kivesz
85 P_lakkozo1 = Plakkozo1_k
86 P_lakkozo2 = Plakkozo2_k
87 P_lakkozo2ki = Plakkozo2ki_k
88 P_szalag = Pszalag
89 P_szalag_felett = Pszalag_felett
90 P_service1 = Pservice1
91 P_service2 = Pservice2
92 '
93 '***************************************************************************
94 'Értékadások
95 '***************************************************************************
96 mcmd_num=M_Inw(10160)          'parancsszám beolvasása
97 seb=M_Inw(10480)               'Sebesség a plc-tõl
98 '***************************************************************************
99 '
100 If mcmd_num <> 0 Then
101 '
102      mcode = -1                                                      'visszatérési érték -1
103 '
104     If mcmd_num > mcmd_cnt Then
105         For mi=1 To 10
106             mpa(mi)=0                                               'paraméterek törlése
107         Next mi
108     '
109         For mi=1 To 9
110             mres(mi)=0                                              'eredmények törlése
111         Next mi
112     '
113         mfinish=0                                                     'subrutin lefutott jel nullázása
114         mcmd_cnt=mcmd_num                                 'parancs számláló növelése
115         mcmd=M_Inw(10176)                                   'parancs beolvasása
116         For mi=1 To 10
117             mpa(mi)=M_Inw(10176+mi*16)                 'paraméterek beolvasása
118         Next mi
119     '
120         mstate=1
121         GoSub *statemachine                                    'ugrás az állapotgépre
122     Else
123         GoSub *statemachine
124     EndIf
125     '
126     If mfinish=1 Then
127          For mi=1 To 9
128              M_Outw(10192+mi*16)=mres(mi)            'eredmények visszaküldése
129          Next mi
130          M_Outw(10192)=mcode                               'hibakód visszaküldése
131          M_Outw(10176)=mcmd                                'parancs visszaküldése
132          M_Outw(10160)=mcmd_num                        'parancsszám visszaküldése
133          Dly 0.1
134          mfinish=0
135          mcmd=0
136          mstate=0
137     EndIf
138 '
139 Else
140 '
141      mcmd_cnt=0                                                    'ha parancsszám 0 -> parancs számláló 0
142 '
143 EndIf
144 '
145 WEnd
146 '
147 '***************************************************************************
148 'Állapotgép
149 '***************************************************************************
150 *statemachine
151 '
152 Select mcmd                                                'parancs végrehajtás, szubrutin hívások
153 Case 0
154              Dly 0.05
155              Break
156 Case 2
157              GoSub *chopen                                  'megfogó nyit
158              Break
159 Case 3
160              GoSub *chclose                                 'megfogó zár
161              Break
162 Case 6
163              GoSub *cgohome                                 'alaphelyzetbe áll
164              Break
165 Case 7
166              GoSub *cgoservice                                 'szervíz helyzetbe áll
167              Break
168 Case 20
169              GoSub *cmoveautofeszek
170              Break
171 Case 21
172              GoSub *cmoveautohajl2be
173              Break
174 Case 22
175              GoSub *cmoveautohajl2ki
176              Break
177 Case 23
178              GoSub *cmoveautohajl1be
179              Break
180 Case 24
181              GoSub *cmoveautohajl1ki
182              Break
183 Case 25
184              GoSub *cmoveautolakk2be
185              Break
186 Case 26
187              GoSub *cmoveautolakk2ki
188              Break
189 Case 27
190              GoSub *cmoveautolakk1be
191              Break
192 Case 28
193              GoSub *cmoveautolakk1ki
194              Break
195 Case 30
196              GoSub *cmoveautoszalag
197              Break
198 Case 31
199              GoSub  *cmoveautohaj2ki2
200              Break
201 Case 32
202              GoSub  *cmoveautohaj1ki2
203              Break
204 Case 33
205              GoSub  *cmoveautohaj2ki4
206              Break
207 Case 34
208              GoSub  *cmoveautohaj1ki4
209              Break
210 Case 40
211              GoSub *cmovefromstation
212              Break
213 Case 41
214              GoSub *cmove2position                          'mozgás célkoordinátákhoz
215              Break
216 Case 51                                                     'abort parancs
217              GoSub *cabort
218              Break
219 Default
220              mfinish=1
221              Break
222 End Select
223 Return
224 '
225 '***************************************************************************
226 'ÜTKÖZÉS
227 '***************************************************************************
228 *deepimpact
229 '
230 ColChk Off
231 Servo On
232 Ovrd 5
233 PEscape = P_ColDir(1) * (-50)     ' új     visszavonulási irány kalkuláció (eddigi mozgással ellentétesen kb. 5 mm)
234 PDestination = P_Fbc(1) + PEscape' új
235 Mvs PDestination' új
236 '*HOpen 1 régi
237 '*Mov P_Curr, -20 régi
238 Servo Off
239 Error 9100
240 '
241 '***************************************************************************
242 'E Stop
243 '***************************************************************************
244 *emergencystop
245 '
246 Servo Off
247 Error 9100
248 '
249 '***************************************************************************
250 'MEGFOGO NYIT
251 '***************************************************************************
252 *chopen
253 '
254 Select mstate
255 Case 1
256               If ((mpa(1) >=1) And (mpa(1)<=2)) Then
257                        mstate=mstate+1
258               Else
259                        mcode= -2
260                        mfinish=1
261               EndIf
262               Break
263 Case 2
264                HOpen mpa(1)
265                mstate=mstate+1
266                Break
267 Case 3
268                If M_In(900+(mpa(1)-1)) = 1 Then
269                       Dly 0.1
270                       mstate=mstate+1
271                EndIf
272                Break
273 Case 4
274                mres(1)=mpa(1)
275                mcode=0
276                mfinish=1
277                Break
278 End Select
279 '
280 Return
281 '
282 '***************************************************************************
283 'MEGFOGO ZÁR
284 '***************************************************************************
285 *chclose
286 '
287 Select mstate
288 Case 1
289               If ((mpa(1)>=1) And (mpa(1)<=2)) Then
290                        mstate=mstate+1
291               Else
292                        mcode= -2
293                        mfinish=1
294               EndIf
295               Break
296 Case 2
297                HClose mpa(1)
298                mstate=mstate+1
299                Break
300 Case 3
301                If M_In(900+(mpa(1)-1)) = 0 Then
302                    Dly 0.1
303                    mstate=mstate+1
304                EndIf
305                Break
306 Case 4
307                mres(1)=mpa(1)
308                mcode=0
309                mfinish=1
310                Break
311 End Select
312 '
313 Return
314 '
315 '******************************************************************************
316 'HOME
317 '******************************************************************************
318 *cgohome
319 '
320 Select mstate
321 Case 1
322               If ((mpa(1)>=1) And (mpa(1)<=2)) Then
323                        mstate=mstate+1
324               Else
325                        mcode= -2
326                        mfinish=1
327               EndIf
328               Break
329 Case 2
330                HOpen 1
331                HOpen 2
332                If (M_In(900) = 1 And M_In(901) = 1) Then
333                    mstate=4
334                EndIf
335                Break
336 Case 3
337                'HOpen 2
338                'If M_In(901) = 1 Then
339                    mstate=mstate+1
340                'EndIf
341                'Break
342 Case 4
343                If Dist(P_Curr,PHome)<5 Then
344                    Mov PHome
345                    mstate=8
346                Else
347                    mstate=mstate+1
348                EndIf
349                Break
350 Case 5
351                If mpa(1)=1 Then Ovrd seb
352                If mpa(1)=2 Then Ovrd seb
353                mstate=mstate+1
354                Break
355 Case 6
356                GoSub *initwhere
357                GoSub *findnearest                               'legközelebbi pont megkeresése
358                If mdis>10 Then                                  'ha a távolság nagyobb mint 10mm
359                    ptmp = P_Curr
360                    ptmp.X = prout(mnpi).X
361                    ptmp.Y = prout(mnpi).Y
362                    ptmp.Z = prout(mnpi).Z
363                    Mvs ptmp
364                    Mov prout(mnpi)                               'mozgás a legközelebbi pontba
365                EndIf
366                mstate=mstate+1
367                Break
368 Case 7
369                mdsti = 1                                          'célpont index
370                GoSub *route                                       'pontról pontra lépkedés a root táblában
371                mstate=mstate+1
372                Break
373 Case 8
374                mcode=0
375                mfinish=1
376                Break
377 End Select
378 '
379 Return
380 '
381 '******************************************************************************
382 'SZERVÍZ POZÍCIÓ
383 '******************************************************************************
384 *cgoservice
385 Select mstate
386 Case 1
387                 Select mpa(1)
388                     Case 1
389                         Ovrd 10
390                         Cnt 1
391                         Mov Pservice1
392                         Mov Pservice1, 100
393                         Cnt 0
394                     Case 2
395                         Ovrd 10
396                         Cnt 1
397                         Mov Pservice2
398                         Mov Pservice2, 100
399                         Cnt 0
400                 End Select
401                 mres(1)=mpa(1)
402                 mcode=0
403                 mfinish=1
404 End Select
405 '
406 Return
407 '
408 '******************************************************************************
409 'MOZGÁS TESZT
410 '******************************************************************************
411 *cmove2position
412 '
413 Select mstate
414 Case 1
415                  If (mpa(1)>=1) And (mpa(1)<=13) Then
416                     mstate=mstate+1
417                  Else
418                     mcode= -2
419                     mfinish=1
420                  EndIf
421                  Break
422 Case 2
423                  Select mpa(1)
424                     Case 1
425                         mstate=3
426                     Case 2
427                         mstate=4
428                     Case 3
429                         mstate=5
430                     Case 4
431                         mstate=6
432                     Case 5
433                         mstate=7
434                     Case 6
435                         mstate=8
436                     Case 7
437                         mstate=9
438                     Case 8
439                         mstate=10
440                     Case 9
441                         mstate=11
442                     Case 10
443                         mstate=12
444                     Case 11
445                         mstate=13
446                     Case 12
447                         mstate=14
448                     Case 13
449                         mstate=15
450                  End Select
451                  Break
452 Case 3
453                  Ovrd seb
454                  Fine 100
455                  Mov PHome
456                  mstate=16
457                  Break
458 Case 4
459                  Ovrd seb
460                  Mov Pfeszek,-150
461                  Fine 100
462                  Mov Pfeszek
463                  mstate=16
464                  Break
465 Case 5
466                  Ovrd seb
467                  Mov Pfeszek_felett,-170
468                  Fine 100
469                  Mov Pfeszek_felett
470                  mstate=16
471                  Break
472 Case 6
473                  Ovrd seb
474                  Mov P_hajl1berak,-150
475                  Fine 100
476                  Mvs P_hajl1berak
477                  mstate=16
478                  Break
479 Case 7
480                  Ovrd seb
481                  Mov Phajl1kivesz,-150
482                  Fine 100
483                  Mov Phajl1kivesz
484                  mstate=16
485                  Break
486 Case 8
487                  Ovrd seb
488                  Mov P_hajl2berak,-150
489                  Fine 100
490                  Mvs P_hajl2berak
491                  mstate=16
492                  Break
493 Case 9
494                  Ovrd seb
495                  Mov Phajl2kivesz,-150
496                  Fine 100
497                  Mov Phajl2kivesz
498                  mstate=16
499                  Break
500 Case 10
501                  Ovrd seb
502                  Fine 100
503                  Mov P_lakkozo1,-150
504                  Fine 100
505                  Mvs P_lakkozo1
506                  mstate=16
507                  Break
508 Case 11
509                  Ovrd seb
510                  Fine 100
511                  Mov Plakkozo1_elott
512                  mstate=16
513                  Break
514 Case 12
515                  Ovrd seb
516                  Fine 100
517                  Mov P_lakkozo2,-150
518                  Mvs P_lakkozo2
519                  mstate=16
520                  Break
521 Case 13
522                  Ovrd seb
523                  Fine 100
524                  Mov Plakkozo2_elott
525                  mstate=16
526                  Break
527 Case 14
528                  Ovrd seb
529                  Fine 100
530                  Mov Pszalag
531                  mstate=16
532                  Break
533 Case 15
534                  Ovrd seb
535                  Fine 100
536                  Mov Pszalag_felett
537                  mstate=16
538                  Break
539 Case 16
540                  mres(1)=mpa(1)
541                  mcode=0
542                  mfinish=1
543                  Break
544 End Select
545 '
546 Return
547 '
548 '******************************************************************************
549 *cabort
550 '
551 mstate=0
552 mcode=0
553 mfinish=1
554 '
555 Return
556 '
557 '******************************************************************************
558 *cmoveautofeszek
559 '
560     Ovrd seb
561     Mov Pfeszek,-20
562     Fine 100
563     Mov Pfeszek
564     HClose 2
565     Dly 0.1
566     Fine 0
567     mcode=0
568     mfinish=1
569 '
570 Return
571 '
572 '******************************************************************************
573 *cmoveautohajl2be
574 '
575     Ovrd seb
576     Cnt 1
577     Mov Pfeszek,-150
578     Mov PHome
579     Mvs Phajl2_elott
580     Cnt 0
581     Fine 100
582     Mvs Phajl2berak_k
583     Fine 0
584     mcode=0
585     mfinish=1
586 '
587 Return
588 '
589 '******************************************************************************
590 *cmoveautohajl2ki
591 '
592     Ovrd seb
593     Cnt 1
594     If Dist(P_Curr,PHome)>0.1 Then
595     Mov P_Curr,-150
596     EndIf
597     Mov Phajl2kivesz,-150
598     Cnt 0
599     Fine 100
600     Mov Phajl2kivesz
601     Fine 0
602     HClose 2
603     Dly 0.1
604     mcode=0
605     mfinish=1
606 '
607 Return
608 '
609 '******************************************************************************
610 *cmoveautohajl1be
611 '
612     Ovrd seb
613     Cnt 1
614     Mov Pfeszek,-150
615     Mov PHome
616     Ovrd (seb/1.5)
617     Mov Phajl1_elott
618     Cnt 0
619     Fine 200
620     Mvs Phajl1berak_k
621     Fine 0
622     Ovrd seb
623     mcode=0
624     mfinish=1
625 '
626 Return
627 '
628 '******************************************************************************
629 *cmoveautohajl1ki
630 '
631     Ovrd seb
632     Cnt 1
633     If Dist(P_Curr,PHome)>0.1 Then
634     Mov P_Curr,-150
635     EndIf
636     Mov Phajl1kivesz,-150
637     Cnt 0
638     Fine 100
639     Mov Phajl1kivesz
640     Fine 0
641     HClose 2
642     Dly 0.1
643     mcode=0
644     mfinish=1
645 '
646 Return
647 '
648 '******************************************************************************
649 *cmoveautolakk2be
650 '
651     Ovrd seb
652     Cnt 1
653     If Dist(P_Curr,PHome)>0.1 Then
654     Mov P_Curr,-150
655     EndIf
656     Mov Plakkozok_kozott
657     Mov Plakkozo2_k, -150
658     Cnt 0
659     Fine 200
660     Mvs Plakkozo2_k
661     Fine 0
662     mcode=0
663     mfinish=1
664 '
665 Return
666 '
667 '******************************************************************************
668 *cmoveautolakk2ki
669 '
670     Ovrd seb
671     Cnt 1
672     If Dist(P_Curr,PHome)>0.1 Then
673     Mov P_Curr,-150
674     EndIf
675     Mov Plakkozo2ki_k,-150
676     Cnt 0
677     Fine 200
678     Mvs Plakkozo2ki_k
679     Fine 0
680     HClose 1
681     HClose 2
682     Dly 0.1
683     mcode=0
684     mfinish=1
685 '
686 Return
687 '
688 '******************************************************************************
689 *cmoveautolakk1be
690 '
691     Ovrd seb
692     Cnt 1
693     If Dist(P_Curr,PHome)>0.1 Then
694     Mov P_Curr,-150
695     EndIf
696     Mov Plakk_kozott_uj
697     Mov Plakkozo1_k,-150
698     Cnt 0
699     Fine 200
700     Mvs Plakkozo1_k
701     Fine 0
702     mcode=0
703     mfinish=1
704 '
705 Return
706 '
707 '******************************************************************************
708 *cmoveautolakk1ki
709 '
710     Ovrd seb
711     Cnt 1
712     If Dist(P_Curr,PHome)>0.1 Then
713     Mov P_Curr,-150
714     EndIf
715     Mov Plakkozo1_k,-150
716     Cnt 0
717     Fine 200
718     Mvs Plakkozo1_k
719     Fine 0
720     HClose 1
721     HClose 2
722     Dly 0.1
723     mcode=0
724     mfinish=1
725 '
726 Return
727 '
728 '******************************************************************************
729 *cmoveautoszalag
730 '
731     Ovrd seb
732     Cnt 1
733     If Dist(P_Curr,PHome)>0.1 Then
734         If Dist(P_Curr,Plakkozo2ki_k)<0.1 Then
735             Mov Plakk2_ki_elott
736         Else
737             Mov P_Curr,-150
738         EndIf
739     EndIf
740     Mov Pszalag_felett
741     Cnt 0
742     Ovrd (seb/2)
743     Fine 200
744     Mov Pszalag
745     Fine 0
746     HOpen 2
747     HOpen 1
748     Wait M_In(900) = 1
749     Dly 0.1
750     mcode=0
751     mfinish=1
752 '
753 Return
754 '
755 '******************************************************************************
756 *cmoveautohaj2ki2
757 '
758     Ovrd seb
759     Cnt 1
760     If Dist(P_Curr,PHome)>0.1 Then
761     Mov P_Curr,-150
762     EndIf
763     Mov Phajl2_2_vez1,-150
764     Cnt 0
765     Fine 200
766     Mov Phajl2_2_vez1
767     HClose 1
768     Wait M_In(900) = 0
769     Dly 0.2
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
785     If Dist(P_Curr,PHome)>0.1 Then
786     Mov P_Curr,-150
787     EndIf
788     Mov Phajl1_2_vez1,-150
789     Cnt 0
790     Fine 200
791     Mov Phajl1_2_vez1
792     HClose 1
793     Wait M_In(900) = 0
794     Dly 0.2
795     Ovrd (seb/5)
796     Mov Phajl1kivesz
797     HClose 2
798     Dly 0.1
799     Fine 0
800     mcode=0
801     mfinish=1
802 '
803 Return
804 '
805 '******************************************************************************
806 *cmoveautohaj2ki4
807 '
808     Ovrd seb
809     Cnt 1
810     If Dist(P_Curr,PHome)>0.1 Then
811     Mov P_Curr,-150
812     EndIf
813     Mov Phajl2_2_vez2,-150
814     Cnt 0
815     Fine 200
816     Mov Phajl2_2_vez2
817     HClose 1
818     Dly 0.1
819     Mov Phajl2kivesz
820     HClose 2
821     Dly 0.1
822  Fine 0
823     mcode=0
824     mfinish=1
825 '
826 Return
827 '
828 '******************************************************************************
829 *cmoveautohaj1ki4
830 '
831     Ovrd seb
832     Cnt 1
833     If Dist(P_Curr,PHome)>0.1 Then
834     Mov P_Curr,-150
835     EndIf
836     Mov Phajl1_2_vez2,-150
837     Cnt 0
838     Fine 200
839     Mov Phajl1_2_vez2
840     HClose 1
841     Dly 0.1
842     Mov Phajl1kivesz
843     HClose 2
844     Dly 0.1
845     Fine 0
846     mcode=0
847     mfinish=1
848 '
849 Return
850 '
851 '******************************************************************************
852 *cmovefromstation
853 '
854     Ovrd seb
855     Mov P_Curr,-150
856     mcode=0
857     mfinish=1
858 '
859 Return
860 '
861 '******************************************************************************
862 *findnearest                                    ' megkeresi a robot karhoz a legközelebbi pontot a route táblában
863 mnpi = 1
864 mdis = Dist(P_Curr,prout(1))
865 For mi=2 To mrouter
866            m2 = Dist(P_Curr,prout(mi))          'távolság a rout táblában lévõ pontoktól
867            If (m2<mdis) Then
868                       mdis = m2                 'legközelebbi ponttól való távolság
869                       mnpi = mi                 'legközelebbi pont indexe
870            EndIf
871 Next mi
872 '
873 Return
874 '
875 '******************************************************************************
876 *initwhere             'root táblázat feltöltése hazaúthoz
877 If mrstat=1 Then Return
878 GoSub *initrouter                                        'root tábla törlése
879 mrouter = 15
880 '----------------------------------------------------------------------------------------------------------------------------
881 prout(1) = PHome                                        'pontok beállítása
882 prout(2) = Pfeszek_felett
883 prout(3) = Pfeszek_felett
884 prout(4) = Phajl1_elott
885 prout(5) = Phajl1_elott
886 prout(6) = Phajl2_elott
887 prout(7) = Phajl2_elott
888 prout(8) = Plakkozo1_elott
889 prout(9) = Plakkozo1_elott
890 prout(10) = Plakkozo2_elott
891 prout(11) = Plakkozo2_elott
892 prout(12) = Pszalag * (-50.00,+0.00,-100.00)
893 prout(13) = Pszalag_felett
894 prout(14) = Ptmp_home
895 prout(15) = Pszalag
896 '-----------------------------------------------------------------------------------------------------------------------------
897 mrout(1,1) = 0                                    'rout tábla feltöltése hazaúthoz
898 mrout(2,1) = 3
899 mrout(3,1) = 1
900 mrout(4,1) = 5
901 mrout(5,1) = 1
902 mrout(6,1) = 7
903 mrout(7,1) = 1
904 mrout(8,1) = 9
905 mrout(9,1) = 14
906 mrout(10,1) = 11
907 mrout(11,1) = 1
908 mrout(12,1) = 13
909 mrout(13,1) = 14
910 mrout(14,1) = 1
911 mrout(15,1) = 12
912 '-----------------------------------------------------------------------------------------------------------------------------
913 mrstat=1
914 '
915 Return
916 '
917 '******************************************************************************
918 *initrouter                                                  'router táblát törli
919 mrouter = 1                                                'táblázat méret
920 For m1 = 1 To 15
921 For m2 = 1 To 15
922            mrout(m1,m2) = -1                       'tábla feltöltés: -1
923            If (m1=m2) Then mrout(m1,m2)=0          'átló feltöltés:  0
924 Next m2
925 Next m1
926 '
927 Return
928 '
929 '******************************************************************************
930 *route                                                        'route algoritmus
931  Cnt 1, 50, 50
932 *ghloop:                                                     'lépkedés ponról pontra a root táblában
933               mnext = mrout(mnpi,mdsti)            'következõ pont indexe
934               If  mnext=0 Then
935                        'alaphelyzetbe értünk
936               Else
937                        'még van
938               EndIf
939               If (mnext > 0) Then
940                      Mov prout(mnext) Type 1,0     'következõ pontba megy
941                      mnpi = mnext                        'pontindex átadása
942               EndIf
943 If (mnext > 0) Then GoTo *ghloop
944 Cnt 0
945 mst = mnext
946 '
947 Return
948 '
949 ''******************************************************************************
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
Phajl1berak_k=(+418.00,-174.30,+491.00,-0.78,-84.52,+158.07)(6,1048576)
Phajl1berak=(+418.00,-174.30,+491.00,-0.78,-84.52,+158.07)(6,1048576)
Phajl2berak_k=(+215.00,-344.00,+481.50,+11.66,-77.62,+109.45)(6,0)
Phajl2berak=(+215.00,-344.00,+481.50,+11.66,-77.62,+109.45)(6,0)
Plakkozo1_k=(+332.60,+385.41,+558.60,-136.77,+76.37,-91.95)(6,0)
Plakkozo1=(+332.60,+385.41,+558.60,-136.77,+76.37,-91.95)(6,0)
Plakkozo2_k=(+533.22,+112.48,+532.47,-103.77,+36.76,-109.64)(6,1048576)
Plakkozo2=(+533.22,+112.48,+532.47,-103.77,+36.76,-109.64)(6,1048576)
Plakkozo2ki_k=(+533.22,+112.48,+532.47,-103.66,+36.18,-109.46)(6,0)
Plakkozo2ki=(+533.22,+112.48,+532.47,-103.66,+36.18,-109.46)(6,0)
Phajl1_elott=(+278.76,-114.65,+455.83,+46.98,-86.24,+109.99)(6,0)
Phajl1kivesz=(+416.73,-173.12,+462.54,+46.98,-86.24,+109.99)(6,0)
PSzor=(+0.00,+0.00,-150.00,+0.00,+0.00,+0.00,+0.00,+0.00)(6,0)
Phajl2_elott=(+132.64,-209.48,+462.04,+28.25,-87.85,+92.61)(6,0)
Phajl2kivesz=(+209.58,-338.15,+467.00,+28.25,-87.85,+92.61)(6,0)
Plakkozo2_elott=(+388.83,+141.32,+561.07,-103.77,+36.76,-109.64)(6,1048576)
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
ptmp=(+388.83,+141.32,+561.07,-103.77,+36.76,-109.64,+0.00,+0.00)(6,1048576)
Pfeszek_felett=(-29.63,-545.72,+368.18,-178.98,-1.22,+0.36)(7,0)
Plakkozo1_elott=(+230.50,+274.28,+581.09,+177.72,+80.84,-136.71)(6,0)
Plakkozok_kozott=(+237.98,-185.15,+508.35,-84.61,+40.79,-136.87)(6,1048576)
Plakk_kozott_uj=(+314.19,-27.60,+598.82,+90.86,-1.48,+85.05)(6,0)
Plakk2_ki_elott=(+321.91,+106.66,+561.27,-90.01,+41.93,-90.01)(6,0)
Phajl2_2_vez1=(+201.56,-324.08,+513.71,+2.33,-64.27,+118.65)(6,0)
Phajl1_2_vez1=(+403.11,-166.09,+501.05,+8.32,-70.29,+148.92)(6,0)
Phajl2_2_vez2=(+204.15,-330.73,+450.00,+4.35,-76.49,+116.52)(6,0)
Phajl1_2_vez2=(+416.57,-173.30,+460.98,+41.34,-85.75,+115.51)(6,0)
Ptmp_home=(+321.77,-49.22,+488.13,+43.27,-85.79,+128.35,+0.00,+0.00)(6,0)
Jmargit=(-89.56,-22.11,+153.77,+0.66,-41.66,+179.51)
