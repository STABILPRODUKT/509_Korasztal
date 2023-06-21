1 '*************************      509 Körasztal      *************************
2 '***************************************************************************
3 'PDestination és PEscape - ütközáskor használjuk
4 'Pfészek - megfogási pont
5 'Phajl1_elott - számolt poz (Pszor)
6 'Phajl1berak - Mindegyik terméknél egyforma
7 'Phajl1_2_vez1 és 2 - alsó megfogó megfog és lehúz
8 'Phajl1kivesz - utolsó pont, ahol a felsõ pofa megfog
9 'Plakkozó_elott - számolt poz (PSzor)
10 'PSzor - eltolás
11 'P_xxx - nem felvett pontok, értéket kapnak
12 '***************************************************************************
13 'DEKLARÁCIÓ
14 '***************************************************************************
15 '
16 Dim mpa(10)               'paraméter tömb
17 Dim mres(9)               'eredmény tömb
18 Dim prout(15)             'pontlista
19 Dim mrout(15,15)          'rout tábla
20 '***************************************************************************
21 'ALAPHELYZET
22 '***************************************************************************
23 Accel 70,70
24 Spd 100000&
25 Ovrd 5
26 HOpen 1
27 HOpen 2
28 For mi=0 To 11
29 M_Outw(10160+mi*16)=0       'Kimenetek inicializálása
30 Next mi
31 mcmd_cnt=0                              'parancs számláló
32 mcmd_num=0                            'parancs szám
33 mcmd=0                                    'parancs
34 mrouter=1                                 'root táblázat méret
35 mrstat=0                                   'root táblát inicializálni kell/nemkell
36 mdis=1                                      'távolság a legközelebbi ponttól
37 '***************************************************************************
38 'ÜTKÖZÉSVÉDELEM
39 '***************************************************************************
40 Def Act 5,M_ColSts(1)=1 GoTo *deepimpact,S
41 Act 5=1
42 ColChk On, NOErr
43 '***************************************************************************
44 'Vészleállás
45 '***************************************************************************
46 EStop=M_Inw(10544)
47 Def Act 2,M_Inw(10544)>0 GoTo *emergencystop,S
48 Act 2=1
49 '***************************************************************************
50 'FÖPROGRAM CIKLUS
51 '***************************************************************************
52 While 1
53 Jmargit=PtoJ(PHome)
54 '
55 '***************************************************************************
56 'Pozíciók
57 '***************************************************************************
58 '***************************************************************************
59 P_service1 = Pservice1
60 P_service2 = Pservice2
61 P_home = PHome
62 P_feszek = Pfeszek
63 P_hajl1berak = Phajl1berak
64 P_hajl1kivesz = Phajl1kivesz
65 P_hajl2berak =  Phajl2berak
66 P_hajl2kivesz = Phajl2kivesz
67 P_lakkozo1_uj = Plakkozo1_uj
68 P_lakkozo2 = Plakkozo2
69 P_lakkozo2_uj = Plakkozo2_uj
70 P_szalag_uj = Pszalag_uj
71 P_szfelett_uj = Psz_felett_uj
72 P_hajl1berak.Z = Phajl1berak.Z +(M_Inw(10560)/1000)
73 P_hajl2berak.Z = Phajl2berak.Z +(M_Inw(10576)/1000)
74 P_lakkozo1_uj.Z = Plakkozo1_uj.Z +(M_Inw(10592)/1000)
75 P_lakkozo2.Z = Plakkozo2.Z +(M_Inw(10608)/1000)
76 P_lakkozo2_uj.Z = Plakkozo2_uj.Z +(M_Inw(10608)/1000)
77 Phajl1_elott = Phajl1kivesz * PSzor
78 Phajl2_elott = Phajl2kivesz * PSzor
79 P_lakkozo2_elott = Plakkozo2 * PSzor
80 '***************************************************************************
81 '***************************************************************************
82 mcmd_num=M_Inw(10160)          'parancsszám beolvasása
83 seb=M_Inw(10480)               'Sebesség a plc-tõl
84 mNR= M_Inw(10512)              'M_Inw(10482)****Paletta mód amíg nincs a helyén az asztal****
85 '***************************************************************************
86 If mcmd_num <> 0 Then
87 '
88      mcode = -1                                                      'visszatérési érték -1
89 '
90     If mcmd_num > mcmd_cnt Then
91         For mi=1 To 10
92             mpa(mi)=0                                               'paraméterek törlése
93         Next mi
94     '
95         For mi=1 To 9
96             mres(mi)=0                                              'eredmények törlése
97         Next mi
98     '
99         mfinish=0                                                     'subrutin lefutott jel nullázása
100         mcmd_cnt=mcmd_num                                 'parancs számláló növelése
101         mcmd=M_Inw(10176)                                   'parancs beolvasása
102         For mi=1 To 10
103             mpa(mi)=M_Inw(10176+mi*16)                 'paraméterek beolvasása
104         Next mi
105     '
106         mstate=1
107         GoSub *statemachine                                    'ugrás az állapotgépre
108     Else
109         GoSub *statemachine
110     EndIf
111     '
112     If mfinish=1 Then
113          For mi=1 To 9
114              M_Outw(10192+mi*16)=mres(mi)            'eredmények visszaküldése
115          Next mi
116          M_Outw(10192)=mcode                               'hibakód visszaküldése
117          M_Outw(10176)=mcmd                                'parancs visszaküldése
118          M_Outw(10160)=mcmd_num                        'parancsszám visszaküldése
119          Dly 0.1
120          mfinish=0
121          mcmd=0
122          mstate=0
123     EndIf
124 '
125 Else
126 '
127      mcmd_cnt=0                                                    'ha parancsszám 0 -> parancs számláló 0
128 '
129 EndIf
130 '
131 WEnd
132 '
133 '***************************************************************************
134 'Állapotgép
135 '***************************************************************************
136 *statemachine
137 Select mcmd                                                'parancs végrehajtás, szubrutin hívások
138 Case 0
139              Dly 0.05
140              Break
141 Case 2
142              GoSub *chopen                                  'megfogó nyit
143              Break
144 Case 3
145              GoSub *chclose                                 'megfogó zár
146              Break
147 Case 6
148              GoSub *cgohome                                 'alaphelyzetbe áll
149              Break
150 Case 7
151              GoSub *cgoservice                                 'szervíz helyzetbe áll
152              Break
153 Case 20
154              GoSub *cmoveautofeszek
155              Break
156 Case 21
157              GoSub *cmoveautohajl2be
158              Break
159 Case 22
160              GoSub *cmoveautohajl2ki
161              Break
162 Case 23
163              GoSub *cmoveautohajl1be
164              Break
165 Case 24
166              GoSub *cmoveautohajl1ki
167              Break
168 Case 25
169              GoSub *cmoveautolakk2be
170              Break
171 Case 26
172              GoSub *cmoveautolakk2ki
173              Break
174 Case 27
175              GoSub *cmoveautolakk1be
176              Break
177 Case 28
178              GoSub *cmoveautolakk1ki
179              Break
180 Case 30
181              GoSub *cmoveautoszalag
182              Break
183 Case 31
184              GoSub  *cmoveautohaj2ki2
185              Break
186 Case 32
187              GoSub  *cmoveautohaj1ki2
188              Break
189 Case 33
190              GoSub  *cmoveautohaj2ki4
191              Break
192 Case 34
193              GoSub  *cmoveautohaj1ki4
194              Break
195 Case 40
196              GoSub *cmovefromstation
197              Break
198 Case 41
199              GoSub *cmove2position                          'mozgás célkoordinátákhoz
200              Break
201 Case 51                                                     'abort parancs
202              GoSub *cabort
203              Break
204 Default
205              mfinish=1
206              Break
207 End Select
208 Return
209 '
210 '***************************************************************************
211 'ÜTKÖZÉS
212 '***************************************************************************
213 *deepimpact
214 ColChk Off
215 Servo On
216 Ovrd 5
217 PEscape = P_ColDir(1) * (-50)     ' új     visszavonulási irány kalkuláció (eddigi mozgással ellentétesen kb. 5 mm)
218 PDestination = P_Fbc(1) + PEscape' új
219 Mvs PDestination' új
220 '*HOpen 1 régi
221 '*Mov P_Curr, -20 régi
222 Servo Off
223 Error 9100
224 '***************************************************************************
225 'E Stop
226 '***************************************************************************
227 *emergencystop
228 Servo Off
229 Error 9100
230 '***************************************************************************
231 'MEGFOGO NYIT
232  '******************************************************************************
233 *chopen
234 '
235 Select mstate
236 Case 1
237               If ((mpa(1) >=1) And (mpa(1)<=2)) Then
238                        mstate=mstate+1
239               Else
240                        mcode= -2
241                        mfinish=1
242               EndIf
243               Break
244 Case 2
245                HOpen mpa(1)
246                mstate=mstate+1
247                Break
248 Case 3
249                If M_In(900+(mpa(1)-1)) = 1 Then
250                       Dly 0.1
251                       mstate=mstate+1
252                EndIf
253                Break
254 Case 4
255                mres(1)=mpa(1)
256                mcode=0
257                mfinish=1
258                Break
259 End Select
260 '
261 Return
262 '
263 '***************************************************************************
264 'MEGFOGO ZÁR
265  '******************************************************************************
266 *chclose
267 '
268 Select mstate
269 Case 1
270               If ((mpa(1)>=1) And (mpa(1)<=2)) Then
271                        mstate=mstate+1
272               Else
273                        mcode= -2
274                        mfinish=1
275               EndIf
276               Break
277 Case 2
278                HClose mpa(1)
279                mstate=mstate+1
280                Break
281 Case 3
282                If M_In(900+(mpa(1)-1)) = 0 Then
283                    Dly 0.1
284                    mstate=mstate+1
285                EndIf
286                Break
287 Case 4
288                mres(1)=mpa(1)
289                mcode=0
290                mfinish=1
291                Break
292 End Select
293 '
294 Return
295 '
296 '******************************************************************************
297 *cgohome
298 '
299 Select mstate
300 Case 1
301               If ((mpa(1)>=1) And (mpa(1)<=2)) Then
302                        mstate=mstate+1
303               Else
304                        mcode= -2
305                        mfinish=1
306               EndIf
307               Break
308 Case 2
309                HOpen 1
310                HOpen 2
311                If (M_In(900) = 1 And M_In(901) = 1) Then
312                    mstate=4
313                EndIf
314                Break
315 Case 3
316                'HOpen 2
317                'If M_In(901) = 1 Then
318                    mstate=mstate+1
319                'EndIf
320                'Break
321 Case 4
322                If Dist(P_Curr,PHome)<5 Then
323                    Mov PHome
324                    mstate=8
325                Else
326                    mstate=mstate+1
327                EndIf
328                Break
329 Case 5
330                If mpa(1)=1 Then Ovrd seb
331                If mpa(1)=2 Then Ovrd seb
332                mstate=mstate+1
333                Break
334 Case 6
335                GoSub *initwhere
336                GoSub *findnearest                               'legközelebbi pont megkeresése
337                If mdis>10 Then                                  'ha a távolság nagyobb mint 10mm
338                    ptmp = P_Curr
339                    ptmp.X = prout(mnpi).X
340                    ptmp.Y = prout(mnpi).Y
341                    ptmp.Z = prout(mnpi).Z
342                    Mvs ptmp
343                    Mov prout(mnpi)                               'mozgás a legközelebbi pontba
344                EndIf
345                mstate=mstate+1
346                Break
347 Case 7
348                mdsti = 1                                          'célpont index
349                GoSub *route                                       'pontról pontra lépkedés a root táblában
350                mstate=mstate+1
351                Break
352 Case 8
353                mcode=0
354                mfinish=1
355                Break
356 End Select
357 '
358 Return
359 '
360 '******************************************************************************
361 'SZERVÍZ POZÍCIÓ
362 '******************************************************************************
363 *cgoservice
364 Select mstate
365 Case 1
366                 Select mpa(1)
367                     Case 1
368                         Ovrd 10
369                         Cnt 1
370                         Mov Pservice1
371                         Mov Pservice1, 100
372                         Cnt 0
373                     Case 2
374                         Ovrd 10
375                         Cnt 1
376                         Mov Pservice2
377                         Mov Pservice2, 100
378                         Cnt 0
379                 End Select
380                 mres(1)=mpa(1)
381                 mcode=0
382                 mfinish=1
383 End Select
384 '
385 Return
386 '
387 '******************************************************************************
388 'MOZGÁS TESZT
389 '******************************************************************************
390 *cmove2position
391 '
392 Select mstate
393 Case 1
394                  If (mpa(1)>=1) And (mpa(1)<=13) Then
395                     mstate=mstate+1
396                  Else
397                     mcode= -2
398                     mfinish=1
399                  EndIf
400                  Break
401 Case 2
402                  Select mpa(1)
403                     Case 1
404                         mstate=3
405                     Case 2
406                         mstate=4
407                     Case 3
408                         mstate=5
409                     Case 4
410                         mstate=6
411                     Case 5
412                         mstate=7
413                     Case 6
414                         mstate=8
415                     Case 7
416                         mstate=9
417                     Case 8
418                         mstate=10
419                     Case 9
420                         mstate=11
421                     Case 10
422                         mstate=12
423                     Case 11
424                         mstate=13
425                     Case 12
426                         mstate=14
427                     Case 13
428                         mstate=15
429                  End Select
430                  Break
431 Case 3
432                  Ovrd seb
433                  Fine 100
434                  Mov PHome
435                  mstate=16
436                  Break
437 Case 4
438                  Ovrd seb
439                  Mov Pfeszek,-150
440                  Fine 100
441                  Mov Pfeszek
442                  mstate=16
443                  Break
444 Case 5
445                  Ovrd seb
446                  Mov Pfeszek_felett,-170
447                  Fine 100
448                  Mov Pfeszek_felett
449                  mstate=16
450                  Break
451 Case 6
452                  Ovrd seb
453                  Mov P_hajl1berak,-150
454                  Fine 100
455                  Mvs P_hajl1berak
456                  mstate=16
457                  Break
458 Case 7
459                  Ovrd seb
460                  Mov Phajl1kivesz,-150
461                  Fine 100
462                  Mov Phajl1kivesz
463                  mstate=16
464                  Break
465 Case 8
466                  Ovrd seb
467                  Mov P_hajl2berak,-150
468                  Fine 100
469                  Mvs P_hajl2berak
470                  mstate=16
471                  Break
472 Case 9
473                  Ovrd seb
474                  Mov Phajl2kivesz,-150
475                  Fine 100
476                  Mov Phajl2kivesz
477                  mstate=16
478                  Break
479 Case 10
480                  Ovrd seb
481                  Fine 100
482                  Mov P_lakkozo1,-150
483                  Fine 100
484                  Mvs P_lakkozo1
485                  mstate=16
486                  Break
487 Case 11
488                  Ovrd seb
489                  Fine 100
490                  Mov Plakkozo1_elott
491                  mstate=16
492                  Break
493 Case 12
494                  Ovrd seb
495                  Fine 100
496                  Mov P_lakkozo2,-150
497                  Mvs P_lakkozo2
498                  mstate=16
499                  Break
500 Case 13
501                  Ovrd seb
502                  Fine 100
503                  Mov Plakkozo2_elott
504                  mstate=16
505                  Break
506 Case 14
507                  Ovrd seb
508                  Fine 100
509                  Mov Pszalag
510                  mstate=16
511                  Break
512 Case 15
513                  Ovrd seb
514                  Fine 100
515                  Mov Pszalag_felett
516                  mstate=16
517                  Break
518 Case 16
519                  mres(1)=mpa(1)
520                  mcode=0
521                  mfinish=1
522                  Break
523 End Select
524 '
525 Return
526 '
527 '******************************************************************************
528 *cabort
529 '
530 mstate=0
531 mcode=0
532 mfinish=1
533 '
534 Return
535 '
536 '******************************************************************************
537 *cmoveautofeszek
538 '
539     Ovrd seb
540     Mov Pfeszek,-20
541     Fine 100
542     Mov Pfeszek
543     HClose 2
544     Dly 0.1
545     Fine 0
546     mcode=0
547     mfinish=1
548 '
549 Return
550 '
551 '******************************************************************************
552 *cmoveautohajl2be
553 '
554     Ovrd seb
555     Cnt 1
556     Mov Pfeszek,-150
557     Mov PHome
558     Mvs Phajl2_elott
559     Cnt 0
560     Fine 100
561     Mvs P_hajl2berak
562     Fine 0
563     mcode=0
564     mfinish=1
565 '
566 Return
567 '
568 '******************************************************************************
569 *cmoveautohajl2ki
570 '
571     Ovrd seb
572     Cnt 1
573     If Dist(P_Curr,P_home)>0.1 Then
574     Mov P_Curr,-150
575     EndIf
576     Mov Phajl2kivesz,-150
577     Cnt 0
578     Fine 100
579     Mov Phajl2kivesz
580     Fine 0
581     HClose 2
582     Dly 0.1
583     mcode=0
584     mfinish=1
585 '
586 Return
587 '
588 '******************************************************************************
589 *cmoveautohajl1be
590 '
591     Ovrd seb
592     Cnt 1
593     Mov Pfeszek,-150
594     Mov PHome
595  Ovrd (seb/1.5)
596     Mov Phajl1_elott
597     Cnt 0
598     Fine 200
599     Mvs P_hajl1berak
600     Fine 0
601  Ovrd seb
602     mcode=0
603     mfinish=1
604 '
605 Return
606 '
607 '******************************************************************************
608 *cmoveautohajl1ki
609 '
610     Ovrd seb
611     Cnt 1
612     If Dist(P_Curr,P_home)>0.1 Then
613     Mov P_Curr,-150
614     EndIf
615     Mov Phajl1kivesz,-150
616     Cnt 0
617     Fine 100
618     Mov Phajl1kivesz
619     Fine 0
620     HClose 2
621     Dly 0.1
622     mcode=0
623     mfinish=1
624 '
625 Return
626 '
627 '******************************************************************************
628 *cmoveautolakk2be
629 '
630     Ovrd seb
631     Cnt 1
632     If Dist(P_Curr,P_home)>0.1 Then
633     Mov P_Curr,-150
634     EndIf
635     Mov Plakkozok_kozott
636     Mov P_lakkozo2, -150
637     Cnt 0
638     Fine 200
639     Mvs P_lakkozo2
640     Fine 0
641     mcode=0
642     mfinish=1
643 '
644 Return
645 '
646 '******************************************************************************
647 *cmoveautolakk2ki
648 '
649     Ovrd seb
650     Cnt 1
651     If Dist(P_Curr,P_home)>0.1 Then
652     Mov P_Curr,-150
653     EndIf
654     Mov P_lakkozo2_uj,-150
655     Cnt 0
656     Fine 200
657     Mvs P_lakkozo2_uj
658     Fine 0
659     HClose 1
660     HClose 2
661     Dly 0.1
662     mcode=0
663     mfinish=1
664 '
665 Return
666 '
667 '******************************************************************************
668 *cmoveautolakk1be
669 '
670     Ovrd seb
671     Cnt 1
672     If Dist(P_Curr,P_home)>0.1 Then
673     Mov P_Curr,-150
674     EndIf
675     Mov Plakk_kozott_uj
676     'Mov Plakkozok_kozott
677     Mov P_lakkozo1_uj,-150
678     Cnt 0
679     Fine 200
680     Mvs P_lakkozo1_uj
681     Fine 0
682     mcode=0
683     mfinish=1
684 '
685 Return
686 '
687 '******************************************************************************
688 *cmoveautolakk1ki
689 '
690     Ovrd seb
691     Cnt 1
692     If Dist(P_Curr,P_home)>0.1 Then
693     Mov P_Curr,-150
694     EndIf
695     Mov P_lakkozo1_uj,-150
696     Cnt 0
697     Fine 200
698     Mvs P_lakkozo1_uj
699     Fine 0
700     HClose 1
701     HClose 2
702     Dly 0.1
703     mcode=0
704     mfinish=1
705 '
706 Return
707 '
708 '******************************************************************************
709 *cmoveautoszalag
710 '
711     Ovrd seb
712     Cnt 1
713     If Dist(P_Curr,P_home)>0.1 Then
714         If Dist(P_Curr,Plakkozo2_ki_uj)<0.1 Then
715             Mov Plakk2_ki_elott
716         Else
717             Mov P_Curr,-150
718         EndIf
719     EndIf
720     Mov Psz_felett_uj
721     'Mov Pszalag_felett
722     Cnt 0
723  Ovrd (seb/2)
724     Fine 200
725     Mov Pszalag_uj
726     Fine 0
727     HOpen 2
728     HOpen 1
729  Wait M_In(900) = 1
730     Dly 0.1
731     mcode=0
732     mfinish=1
733 '
734 Return
735 '
736 '******************************************************************************
737 *cmoveautohaj2ki2
738 '
739     Ovrd seb
740     Cnt 1
741     If Dist(P_Curr,P_home)>0.1 Then
742     Mov P_Curr,-150
743     EndIf
744     Mov Phajl2_2_vez1,-150
745     Cnt 0
746     Fine 200
747     Mov Phajl2_2_vez1
748     HClose 1
749     Wait M_In(900) = 0
750     Dly 0.2
751     'Mov Phajl2_2_vez2
752     Ovrd (seb/5)
753     Mvs Phajl2kivesz
754     HClose 2
755     Dly 0.1
756     Fine 0
757     mcode=0
758     mfinish=1
759 '
760 Return
761 '
762 '******************************************************************************
763 *cmoveautohaj1ki2
764 '
765     Ovrd seb
766     Cnt 1
767     If Dist(P_Curr,P_home)>0.1 Then
768     Mov P_Curr,-150
769     EndIf
770     Mov Phajl1_2_vez1,-150
771     Cnt 0
772     Fine 200
773     Mov Phajl1_2_vez1
774     HClose 1
775  Wait M_In(900) = 0
776     Dly 0.2
777     'Mov Phajl1_2_vez2
778  Ovrd (seb/5)
779     Mov Phajl1kivesz
780     HClose 2
781     Dly 0.1
782     Fine 0
783     mcode=0
784     mfinish=1
785 '
786 Return
787 '
788 '******************************************************************************
789 *cmoveautohaj2ki4
790 '
791     Ovrd seb
792     Cnt 1
793     If Dist(P_Curr,P_home)>0.1 Then
794     Mov P_Curr,-150
795     EndIf
796     Mov Phajl2_2_vez2,-150
797     Cnt 0
798     Fine 200
799     Mov Phajl2_2_vez2
800     HClose 1
801     Dly 0.1
802     Mov Phajl2kivesz
803     HClose 2
804     Dly 0.1
805  Fine 0
806     mcode=0
807     mfinish=1
808 '
809 Return
810 '
811 '******************************************************************************
812 *cmoveautohaj1ki4
813 '
814     Ovrd seb
815     Cnt 1
816     If Dist(P_Curr,P_home)>0.1 Then
817     Mov P_Curr,-150
818     EndIf
819     Mov Phajl1_2_vez2,-150
820     Cnt 0
821     Fine 200
822     Mov Phajl1_2_vez2
823     HClose 1
824     Dly 0.1
825     Mov Phajl1kivesz
826     HClose 2
827     Dly 0.1
828     Fine 0
829     mcode=0
830     mfinish=1
831 '
832 Return
833 '
834 '******************************************************************************
835 *cmovefromstation
836 '
837     Ovrd seb
838     Mov P_Curr,-150
839     mcode=0
840     mfinish=1
841 '
842 Return
843 '
844 '******************************************************************************
845 *findnearest                                    ' megkeresi a robot karhoz a legközelebbi pontot a route táblában
846 mnpi = 1
847 mdis = Dist(P_Curr,prout(1))
848 For mi=2 To mrouter
849            m2 = Dist(P_Curr,prout(mi))          'távolság a rout táblában lévõ pontoktól
850            If (m2<mdis) Then
851                       mdis = m2                 'legközelebbi ponttól való távolság
852                       mnpi = mi                 'legközelebbi pont indexe
853            EndIf
854 Next mi
855 '
856 Return
857 '
858 '******************************************************************************
859 *initwhere             'root táblázat feltöltése hazaúthoz
860 If mrstat=1 Then Return
861 GoSub *initrouter                                        'root tábla törlése
862 mrouter = 15
863 '----------------------------------------------------------------------------------------------------------------------------
864 prout(1) = PHome                                        'pontok beállítása
865 prout(2) = Pfeszek_felett
866 prout(3) = Pfeszek_felett
867 prout(4) = Phajl1_elott
868 prout(5) = Phajl1_elott
869 prout(6) = Phajl2_elott
870 prout(7) = Phajl2_elott
871 prout(8) = Plakk1_elott_uj
872 prout(9) = Plakk1_elott_uj
873 prout(10) = P_lakkozo2_elott
874 prout(11) = P_lakkozo2_elott
875 'prout(12) = Pszalag,
876 prout(12) = Pszalag_uj * (-50.00,+0.00,-100.00)
877 'prout(12) = Pszalag * (-50.00,+0.00,-100.00)
878 prout(13) = Psz_felett_uj
879 'prout(13) = Pszalag_felett
880 prout(14) = Ptmp_home
881 prout(15) = Pszalag_uj
882 'prout(15) = Pszalag
883 '-----------------------------------------------------------------------------------------------------------------------------
884 mrout(1,1) = 0                                    'rout tábla feltöltése hazaúthoz
885 mrout(2,1) = 3
886 mrout(3,1) = 1
887 mrout(4,1) = 5
888 mrout(5,1) = 1
889 mrout(6,1) = 7
890 mrout(7,1) = 1
891 mrout(8,1) = 9
892 mrout(9,1) = 14
893 mrout(10,1) = 11
894 mrout(11,1) = 1
895 mrout(12,1) = 13
896 mrout(13,1) = 14
897 mrout(14,1) = 1
898 mrout(15,1) = 12
899 '-----------------------------------------------------------------------------------------------------------------------------
900 mrstat=1
901 '
902 Return
903 '
904 '******************************************************************************
905 *initrouter                                                  'router táblát törli
906 mrouter = 1                                                'táblázat méret
907 For m1 = 1 To 15
908 For m2 = 1 To 15
909            mrout(m1,m2) = -1                       'tábla feltöltés: -1
910            If (m1=m2) Then mrout(m1,m2)=0          'átló feltöltés:  0
911 Next m2
912 Next m1
913 '
914 Return
915 '
916 '******************************************************************************
917 *route                                                        'route algoritmus
918  Cnt 1, 50, 50
919 *ghloop:                                                     'lépkedés ponról pontra a root táblában
920               mnext = mrout(mnpi,mdsti)            'következõ pont indexe
921               If  mnext=0 Then
922                        'alaphelyzetbe értünk
923               Else
924                        'még van
925               EndIf
926               If (mnext > 0) Then
927                      Mov prout(mnext) Type 1,0     'következõ pontba megy
928                      mnpi = mnext                        'pontindex átadása
929               EndIf
930 If (mnext > 0) Then GoTo *ghloop
931 Cnt 0
932 mst = mnext
933 '
934 Return
935 '
936 ''******************************************************************************
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
Phajl1_elott=(+288.70,-117.52,+546.03,-178.84,-79.66,-23.49)(6,0)
PSzor=(+0.00,+0.00,-150.00,+0.00,+0.00,+0.00,+0.00,+0.00)(6,0)
Phajl2_elott=(+157.12,-256.81,+549.46,+173.43,-77.12,-50.54)(6,1048576)
P_lakkozo2_elott=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
PEscape=(-11.31,-50.00,-8.06,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PDestination=(+371.22,-67.94,+535.88,+98.58,+82.02,+63.58,+0.00,+0.00)(6,0)
ptmp=(+230.50,+274.28,+581.09,-136.77,+76.37,-91.95,+0.00,+0.00)(6,0)
Pfeszek_felett=(-29.63,-545.72,+368.18,-178.98,-1.22,+0.36)(7,0)
P_lakkozo1=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
Plakkozo1_elott=(+158.76,+325.14,+548.35,-99.67,+39.92,-60.39)(6,1048576)
Plakkozo2_elott=(+537.06,+117.97,+536.78,-106.37,+35.81,-114.04)(6,0)
Pszalag=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
Pszalag_felett=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
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
Phajl1_elott2=(+349.63,-142.52,+534.02,-179.18,-75.27,-23.14)(6,0)
Phajl2_elott2=(+136.98,-209.88,+464.31,-87.79,+19.96,-148.01)(6,1048576)
Phome2=(+0.43,-297.45,+453.24,+30.71,-88.08,+58.20)(6,0)
Plakkozo1=(+302.12,+434.37,+524.24,-101.40,+39.06,-63.18)(6,1048576)
Plakkozo1_backup=(+301.29,+436.48,+526.08,-103.00,+38.63,-65.73)(6,1048576)
Plakkozo1_u=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
Plakkozo2_backup=(+537.06,+117.97,+536.78,-106.37,+35.81,-114.04)(6,1048576)
Jmargit=(-89.56,-22.11,+153.77,+0.66,-41.66,+179.51)
