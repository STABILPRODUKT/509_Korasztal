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
70 P_home = PHome
71 P_feszek = Pfeszek
72 P_hajl1berak = Phajl1berak
73 P_hajl1kivesz = Phajl1kivesz
74 P_hajl2berak =  Phajl2berak
75 P_hajl2kivesz = Phajl2kivesz
76 P_lakkozo1 = Plakkozo1
77 P_lakkozo2 = Plakkozo2
78 P_szalag = Pszalag
79 P_szalag_felett = Pszalag_felett
80 P_hajl1berak.Z = Phajl1berak.Z +(M_Inw(10560)/1000)
81 P_hajl2berak.Z = Phajl2berak.Z +(M_Inw(10576)/1000)
82 P_lakkozo1.Z = Plakkozo1.Z +(M_Inw(10592)/1000)
83 P_lakkozo2.Z = Plakkozo2.Z +(M_Inw(10608)/1000)
84 P_hajl1_elott = Phajl1kivesz * PSzor
85 'P_hajl1_elott = Phajl1_elott
86 P_hajl2_elott = P_hajl2kivesz * PSzor
87 'P_hajl2_elott = Phajl2_elott
88 P_lakkozo1_elott = Plakkozo1 * PSzor
89 P_lakkozo2_elott = Plakkozo2 * PSzor
90 '***************************************************************************
91 '***************************************************************************
92 mcmd_num=M_Inw(10160)                               'parancsszám beolvasása
93 seb=M_Inw(10480)               ' Sebesség a plc-tõl
94 mNR= M_Inw(10512)          '  M_Inw(10482)****Paletta mód amíg nincs a helyén az asztal****
95 'PLACE = Plt 1,mNR           '****Paletta mód amíg nincs a helyén az asztal****
96 '***************************************************************************
97 If mcmd_num <> 0 Then
98 '
99      mcode = -1                                                      'visszatérési érték -1
100 '
101     If mcmd_num > mcmd_cnt Then
102         For mi=1 To 10
103             mpa(mi)=0                                               'paraméterek törlése
104         Next mi
105     '
106         For mi=1 To 9
107             mres(mi)=0                                              'eredmények törlése
108         Next mi
109     '
110         mfinish=0                                                     'subrutin lefutott jel nullázása
111         mcmd_cnt=mcmd_num                                 'parancs számláló növelése
112         mcmd=M_Inw(10176)                                   'parancs beolvasása
113         For mi=1 To 10
114             mpa(mi)=M_Inw(10176+mi*16)                 'paraméterek beolvasása
115         Next mi
116     '
117         mstate=1
118         GoSub *statemachine                                    'ugrás az állapotgépre
119     Else
120         GoSub *statemachine
121     EndIf
122     '
123     If mfinish=1 Then
124          For mi=1 To 9
125              M_Outw(10192+mi*16)=mres(mi)            'eredmények visszaküldése
126          Next mi
127          M_Outw(10192)=mcode                               'hibakód visszaküldése
128          M_Outw(10176)=mcmd                                'parancs visszaküldése
129          M_Outw(10160)=mcmd_num                        'parancsszám visszaküldése
130          Dly 0.1
131          mfinish=0
132          mcmd=0
133          mstate=0
134     EndIf
135 '
136 Else
137 '
138      mcmd_cnt=0                                                    'ha parancsszám 0 -> parancs számláló 0
139 '
140 EndIf
141 '
142 WEnd
143 '
144 '***************************************************************************
145 'Állapotgép
146 '***************************************************************************
147 *statemachine
148 Select mcmd                                                'parancs végrehajtás, szubrutin hívások
149 Case 0
150              Dly 0.05
151              Break
152 Case 2
153              GoSub *chopen                                  'megfogó nyit
154              Break
155 Case 3
156              GoSub *chclose                                 'megfogó zár
157              Break
158 Case 6
159              GoSub *cgohome                                 'alaphelyzetbe áll
160              Break
161 Case 20
162              GoSub *cmoveautofeszek
163              Break
164 Case 21
165              GoSub *cmoveautohajl2be
166              Break
167 Case 22
168              GoSub *cmoveautohajl2ki
169              Break
170 Case 23
171              GoSub *cmoveautohajl1be
172              Break
173 Case 24
174              GoSub *cmoveautohajl1ki
175              Break
176 Case 25
177              GoSub *cmoveautolakk2be
178              Break
179 Case 26
180              GoSub *cmoveautolakk2ki
181              Break
182 Case 27
183              GoSub *cmoveautolakk1be
184              Break
185 Case 28
186              GoSub *cmoveautolakk1ki
187              Break
188 Case 30
189              GoSub *cmoveautoszalag
190              Break
191 Case 31
192              GoSub  *cmoveautohaj2ki2
193              Break
194 Case 32
195              GoSub  *cmoveautohaj1ki2
196              Break
197 Case 33
198              GoSub  *cmoveautohaj2ki4
199              Break
200 Case 34
201              GoSub  *cmoveautohaj1ki4
202              Break
203 Case 40
204              GoSub *cmovefromstation
205              Break
206 Case 41
207              GoSub *cmove2position                          'mozgás célkoordinátákhoz
208              Break
209 Case 51                                                     'abort parancs
210              GoSub *cabort
211              Break
212 Default
213              mfinish=1
214              Break
215 End Select
216 Return
217 '
218 '***************************************************************************
219 'ÜTKÖZÉS
220 '***************************************************************************
221 *deepimpact
222 ColChk Off
223 Servo On
224 Ovrd 5
225 PEscape = P_ColDir(1) * (-50)     ' új     visszavonulási irány kalkuláció (eddigi mozgással ellentétesen kb. 5 mm)
226 PDestination = P_Fbc(1) + PEscape' új
227 Mvs PDestination' új
228 '*HOpen 1 régi
229 '*Mov P_Curr, -20 régi
230 Servo Off
231 Error 9100
232 '***************************************************************************
233 'E Stop
234 '***************************************************************************
235 *emergencystop
236 Servo Off
237 Error 9100
238 '***************************************************************************
239 'MEGFOGO NYIT
240  '******************************************************************************
241 *chopen
242 '
243 Select mstate
244 Case 1
245               If ((mpa(1) >=1) And (mpa(1)<=2)) Then
246                        mstate=mstate+1
247               Else
248                        mcode= -2
249                        mfinish=1
250               EndIf
251               Break
252 Case 2
253                HOpen mpa(1)
254                mstate=mstate+1
255                Break
256 Case 3
257                If M_In(900+(mpa(1)-1)) = 1 Then
258                       Dly 0.1
259                       mstate=mstate+1
260                EndIf
261                Break
262 Case 4
263                mres(1)=mpa(1)
264                mcode=0
265                mfinish=1
266                Break
267 End Select
268 '
269 Return
270 '
271 '***************************************************************************
272 'MEGFOGO ZÁR
273  '******************************************************************************
274 *chclose
275 '
276 Select mstate
277 Case 1
278               If ((mpa(1)>=1) And (mpa(1)<=2)) Then
279                        mstate=mstate+1
280               Else
281                        mcode= -2
282                        mfinish=1
283               EndIf
284               Break
285 Case 2
286                HClose mpa(1)
287                mstate=mstate+1
288                Break
289 Case 3
290                If M_In(900+(mpa(1)-1)) = 0 Then
291                    Dly 0.1
292                    mstate=mstate+1
293                EndIf
294                Break
295 Case 4
296                mres(1)=mpa(1)
297                mcode=0
298                mfinish=1
299                Break
300 End Select
301 '
302 Return
303 '
304 '******************************************************************************
305 *cgohome
306 '
307 Select mstate
308 Case 1
309               If ((mpa(1)>=1) And (mpa(1)<=2)) Then
310                        mstate=mstate+1
311               Else
312                        mcode= -2
313                        mfinish=1
314               EndIf
315               Break
316 Case 2
317                HOpen 1
318                HOpen 2
319                If (M_In(900) = 1 And M_In(901) = 1) Then
320                    mstate=4
321                EndIf
322                Break
323 Case 3
324                'HOpen 2
325                'If M_In(901) = 1 Then
326                    mstate=mstate+1
327                'EndIf
328                'Break
329 Case 4
330                If Dist(P_Curr,PHome)<5 Then
331                    Mov PHome
332                    mstate=8
333                Else
334                    mstate=mstate+1
335                EndIf
336                Break
337 Case 5
338                If mpa(1)=1 Then Ovrd seb
339                If mpa(1)=2 Then Ovrd seb
340                mstate=mstate+1
341                Break
342 Case 6
343                GoSub *initwhere
344                GoSub *findnearest                               'legközelebbi pont megkeresése
345                If mdis>10 Then                                  'ha a távolság nagyobb mint 10mm
346                    ptmp = P_Curr
347                    ptmp.X = prout(mnpi).X
348                    ptmp.Y = prout(mnpi).Y
349                    ptmp.Z = prout(mnpi).Z
350                    Mvs ptmp
351                    Mov prout(mnpi)                               'mozgás a legközelebbi pontba
352                EndIf
353                mstate=mstate+1
354                Break
355 Case 7
356                mdsti = 1                                          'célpont index
357                GoSub *route                                       'pontról pontra lépkedés a root táblában
358                mstate=mstate+1
359                Break
360 Case 8
361                mcode=0
362                mfinish=1
363                Break
364 End Select
365 '
366 Return
367 '
368 '******************************************************************************
369 'MOZGÁS TESZT
370 '******************************************************************************
371 *cmove2position
372 '
373 Select mstate
374 Case 1
375                  If (mpa(1)>=1) And (mpa(1)<=13) Then
376                     mstate=mstate+1
377                  Else
378                     mcode= -2
379                     mfinish=1
380                  EndIf
381                  Break
382 Case 2
383                  Select mpa(1)
384                     Case 1
385                         mstate=3
386                     Case 2
387                         mstate=4
388                     Case 3
389                         mstate=5
390                     Case 4
391                         mstate=6
392                     Case 5
393                         mstate=7
394                     Case 6
395                         mstate=8
396                     Case 7
397                         mstate=9
398                     Case 8
399                         mstate=10
400                     Case 9
401                         mstate=11
402                     Case 10
403                         mstate=12
404                     Case 11
405                         mstate=13
406                     Case 12
407                         mstate=14
408                     Case 13
409                         mstate=15
410                  End Select
411                  Break
412 Case 3
413                  Ovrd seb
414                  Fine 100
415                  Mov PHome
416                  mstate=16
417                  Break
418 Case 4
419                  Ovrd seb
420                  Mov Pfeszek,-150
421                  Fine 100
422                  Mov Pfeszek
423                  mstate=16
424                  Break
425 Case 5
426                  Ovrd seb
427                  Mov Pfeszek_felett,-170
428                  Fine 100
429                  Mov Pfeszek_felett
430                  mstate=16
431                  Break
432 Case 6
433                  Ovrd seb
434                  Mov P_hajl1berak,-150
435                  Fine 100
436                  Mvs P_hajl1berak
437                  mstate=16
438                  Break
439 Case 7
440                  Ovrd seb
441                  Mov Phajl1kivesz,-150
442                  Fine 100
443                  Mov Phajl1kivesz
444                  mstate=16
445                  Break
446 Case 8
447                  Ovrd seb
448                  Mov P_hajl2berak,-150
449                  Fine 100
450                  Mvs P_hajl2berak
451                  mstate=16
452                  Break
453 Case 9
454                  Ovrd seb
455                  Mov Phajl2kivesz,-150
456                  Fine 100
457                  Mov Phajl2kivesz
458                  mstate=16
459                  Break
460 Case 10
461                  Ovrd seb
462                  Fine 100
463                  Mov P_lakkozo1,-150
464                  Fine 100
465                  Mvs P_lakkozo1
466                  mstate=16
467                  Break
468 Case 11
469                  Ovrd seb
470                  Fine 100
471                  Mov Plakkozo1_elott
472                  mstate=16
473                  Break
474 Case 12
475                  Ovrd seb
476                  Fine 100
477                  Mov P_lakkozo2,-150
478                  Mvs P_lakkozo2
479                  mstate=16
480                  Break
481 Case 13
482                  Ovrd seb
483                  Fine 100
484                  Mov Plakkozo2_elott
485                  mstate=16
486                  Break
487 Case 14
488                  Ovrd seb
489                  Fine 100
490                  Mov Pszalag
491                  mstate=16
492                  Break
493 Case 15
494                  Ovrd seb
495                  Fine 100
496                  Mov Pszalag_felett
497                  mstate=16
498                  Break
499 Case 16
500                  mres(1)=mpa(1)
501                  mcode=0
502                  mfinish=1
503                  Break
504 End Select
505 '
506 Return
507 '
508 '******************************************************************************
509 *cabort
510 '
511 mstate=0
512 mcode=0
513 mfinish=1
514 '
515 Return
516 '
517 '******************************************************************************
518 *cmoveautofeszek
519 '
520     Ovrd seb
521     Mov Pfeszek,-20
522     Fine 100
523     Mov Pfeszek
524     HClose 2
525     Dly 0.1
526     Fine 0
527     mcode=0
528     mfinish=1
529 '
530 Return
531 '
532 '******************************************************************************
533 *cmoveautohajl2be
534 '
535     Ovrd seb
536     Cnt 1
537     Mov Pfeszek,-150
538     Mov PHome
539     Mvs Phajl2_elott
540     Cnt 0
541     Fine 100
542     Mvs P_hajl2berak
543     Fine 0
544     mcode=0
545     mfinish=1
546 '
547 Return
548 '
549 '******************************************************************************
550 *cmoveautohajl2ki
551 '
552     Ovrd seb
553     Cnt 1
554     If Dist(P_Curr,P_home)>0.1 Then
555     Mov P_Curr,-150
556     EndIf
557     Mov Phajl2kivesz,-150
558     Cnt 0
559     Fine 100
560     Mov Phajl2kivesz
561     Fine 0
562     HClose 2
563     Dly 0.1
564     mcode=0
565     mfinish=1
566 '
567 Return
568 '
569 '******************************************************************************
570 *cmoveautohajl1be
571 '
572     Ovrd seb
573     Cnt 1
574     Mov Pfeszek,-150
575     Mov PHome
576  Ovrd (seb/1.5)
577     Mov Phajl1_elott
578     Cnt 0
579     Fine 200
580     Mvs P_hajl1berak
581     Fine 0
582  Ovrd seb
583     mcode=0
584     mfinish=1
585 '
586 Return
587 '
588 '******************************************************************************
589 *cmoveautohajl1ki
590 '
591     Ovrd seb
592     Cnt 1
593     If Dist(P_Curr,P_home)>0.1 Then
594     Mov P_Curr,-150
595     EndIf
596     Mov Phajl1kivesz,-150
597     Cnt 0
598     Fine 100
599     Mov Phajl1kivesz
600     Fine 0
601     HClose 2
602     Dly 0.1
603     mcode=0
604     mfinish=1
605 '
606 Return
607 '
608 '******************************************************************************
609 *cmoveautolakk2be
610 '
611     Ovrd seb
612     Cnt 1
613     If Dist(P_Curr,P_home)>0.1 Then
614     Mov P_Curr,-150
615     EndIf
616     Mov Plakkozok_kozott
617     Mov P_lakkozo2, -150
618     Cnt 0
619     Fine 200
620     Mvs P_lakkozo2
621     Fine 0
622     mcode=0
623     mfinish=1
624 '
625 Return
626 '
627 '******************************************************************************
628 *cmoveautolakk2ki
629 '
630     Ovrd seb
631     Cnt 1
632     If Dist(P_Curr,P_home)>0.1 Then
633     Mov P_Curr,-150
634     EndIf
635     Mov P_lakkozo2,-150
636     Cnt 0
637     Fine 200
638     Mvs P_lakkozo2
639     Fine 0
640     HClose 1
641     HClose 2
642     Dly 0.1
643     mcode=0
644     mfinish=1
645 '
646 Return
647 '
648 '******************************************************************************
649 *cmoveautolakk1be
650 '
651     Ovrd seb
652     Cnt 1
653     If Dist(P_Curr,P_home)>0.1 Then
654     Mov P_Curr,-150
655     EndIf
656     Mov Plakkozok_kozott
657     Mov P_lakkozo1,-200
658     Cnt 0
659     Fine 200
660     Mvs P_lakkozo1
661     Fine 0
662     mcode=0
663     mfinish=1
664 '
665 Return
666 '
667 '******************************************************************************
668 *cmoveautolakk1ki
669 '
670     Ovrd seb
671     Cnt 1
672     If Dist(P_Curr,P_home)>0.1 Then
673     Mov P_Curr,-150
674     EndIf
675     Mov P_lakkozo1,-150
676     Cnt 0
677     Fine 200
678     Mvs P_lakkozo1
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
689 *cmoveautoszalag
690 '
691     Ovrd seb
692     Cnt 1
693     If Dist(P_Curr,P_home)>0.1 Then
694     Mov P_Curr,-150
695     EndIf
696     Mov Pszalag_felett
697     Cnt 0
698  Ovrd (seb/2)
699     Fine 200
700     Mov Pszalag 'PLACE     'Pszalag ****Paletta mód amíg nincs a helyén az asztal****
701     Fine 0
702     HOpen 2
703     HOpen 1
704  Wait M_In(900) = 1
705     Dly 0.1
706     mcode=0
707     mfinish=1
708 '
709 Return
710 '
711 '******************************************************************************
712 *cmoveautohaj2ki2
713 '
714     Ovrd seb
715     Cnt 1
716     If Dist(P_Curr,P_home)>0.1 Then
717     Mov P_Curr,-150
718     EndIf
719     Mov Phajl2_2_vez1,-150
720     Cnt 0
721     Fine 200
722     Mov Phajl2_2_vez1
723     HClose 1
724     Wait M_In(900) = 0
725     Dly 0.2
726     'Mov Phajl2_2_vez2
727     Ovrd (seb/5)
728     Mvs Phajl2kivesz
729     HClose 2
730     Dly 0.1
731     Fine 0
732     mcode=0
733     mfinish=1
734 '
735 Return
736 '
737 '******************************************************************************
738 *cmoveautohaj1ki2
739 '
740     Ovrd seb
741     Cnt 1
742     If Dist(P_Curr,P_home)>0.1 Then
743     Mov P_Curr,-150
744     EndIf
745     Mov Phajl1_2_vez1,-150
746     Cnt 0
747     Fine 200
748     Mov Phajl1_2_vez1
749     HClose 1
750  Wait M_In(900) = 0
751     Dly 0.2
752     'Mov Phajl1_2_vez2
753  Ovrd (seb/5)
754     Mov Phajl1kivesz
755     HClose 2
756     Dly 0.1
757     Fine 0
758     mcode=0
759     mfinish=1
760 '
761 Return
762 '
763 '******************************************************************************
764 *cmoveautohaj2ki4
765 '
766     Ovrd seb
767     Cnt 1
768     If Dist(P_Curr,P_home)>0.1 Then
769     Mov P_Curr,-150
770     EndIf
771     Mov Phajl2_2_vez2,-150
772     Cnt 0
773     Fine 200
774     Mov Phajl2_2_vez2
775     HClose 1
776     Dly 0.1
777     Mov Phajl2kivesz
778     HClose 2
779     Dly 0.1
780  Fine 0
781     mcode=0
782     mfinish=1
783 '
784 Return
785 '
786 '******************************************************************************
787 *cmoveautohaj1ki4
788 '
789     Ovrd seb
790     Cnt 1
791     If Dist(P_Curr,P_home)>0.1 Then
792     Mov P_Curr,-150
793     EndIf
794     Mov Phajl1_2_vez2,-150
795     Cnt 0
796     Fine 200
797     Mov Phajl1_2_vez2
798     HClose 1
799     Dly 0.1
800     Mov Phajl1kivesz
801     HClose 2
802     Dly 0.1
803     Fine 0
804     mcode=0
805     mfinish=1
806 '
807 Return
808 '
809 '******************************************************************************
810 *cmovefromstation
811 '
812     Ovrd seb
813     Mov P_Curr,-150
814     mcode=0
815     mfinish=1
816 '
817 Return
818 '
819 '******************************************************************************
820 *findnearest                                    ' megkeresi a robot karhoz a legközelebbi pontot a route táblában
821 mnpi = 1
822 mdis = Dist(P_Curr,prout(1))
823 For mi=2 To mrouter
824            m2 = Dist(P_Curr,prout(mi))          'távolság a rout táblában lévõ pontoktól
825            If (m2<mdis) Then
826                       mdis = m2                 'legközelebbi ponttól való távolság
827                       mnpi = mi                 'legközelebbi pont indexe
828            EndIf
829 Next mi
830 '
831 Return
832 '
833 '******************************************************************************
834 *initwhere             'root táblázat feltöltése hazaúthoz
835 If mrstat=1 Then Return
836 GoSub *initrouter                                        'root tábla törlése
837 mrouter = 15
838 '----------------------------------------------------------------------------------------------------------------------------
839 prout(1) = PHome                                        'pontok beállítása
840 prout(2) = Pfeszek_felett
841 prout(3) = Pfeszek_felett
842 prout(4) = P_hajl1_elott
843 prout(5) = P_hajl1_elott
844 prout(6) = P_hajl2_elott
845 prout(7) = P_hajl2_elott
846 prout(8) = P_lakkozo1_elott
847 prout(9) = P_lakkozo1_elott
848 prout(10) = P_lakkozo2_elott
849 prout(11) = P_lakkozo2_elott
850 'prout(12) = Pszalag,
851 prout(12) = Pszalag * (-50.00,+0.00,-100.00)
852 prout(13) = Pszalag_felett
853 prout(14) = Ptmp_home
854 prout(15) = Pszalag
855 '-----------------------------------------------------------------------------------------------------------------------------
856 mrout(1,1) = 0                                    'rout tábla feltöltése hazaúthoz
857 mrout(2,1) = 3
858 mrout(3,1) = 1
859 mrout(4,1) = 5
860 mrout(5,1) = 1
861 mrout(6,1) = 7
862 mrout(7,1) = 1
863 mrout(8,1) = 9
864 mrout(9,1) = 14
865 mrout(10,1) = 11
866 mrout(11,1) = 1
867 mrout(12,1) = 13
868 mrout(13,1) = 14
869 mrout(14,1) = 1
870 mrout(15,1) = 12
871 '-----------------------------------------------------------------------------------------------------------------------------
872 mrstat=1
873 '
874 Return
875 '
876 '******************************************************************************
877 *initrouter                                                  'router táblát törli
878 mrouter = 1                                                'táblázat méret
879 For m1 = 1 To 15
880 For m2 = 1 To 15
881            mrout(m1,m2) = -1                       'tábla feltöltés: -1
882            If (m1=m2) Then mrout(m1,m2)=0          'átló feltöltés:  0
883 Next m2
884 Next m1
885 '
886 Return
887 '
888 '******************************************************************************
889 *route                                                        'route algoritmus
890  Cnt 1, 50, 50
891 *ghloop:                                                     'lépkedés ponról pontra a root táblában
892               mnext = mrout(mnpi,mdsti)            'következõ pont indexe
893               If  mnext=0 Then
894                        'alaphelyzetbe értünk
895               Else
896                        'még van
897               EndIf
898               If (mnext > 0) Then
899                      Mov prout(mnext) Type 1,0     'következõ pontba megy
900                      mnpi = mnext                        'pontindex átadása
901               EndIf
902 If (mnext > 0) Then GoTo *ghloop
903 Cnt 0
904 mst = mnext
905 '
906 Return
907 '
908 ''******************************************************************************
prout(1)=(+1.27,-251.85,+451.89,+178.98,-90.00,-88.98,+0.00,+0.00)(6,0)
prout(2)=(-29.63,-545.72,+368.18,-178.98,-1.22,+0.36,+0.00,+0.00)(7,0)
prout(3)=(-29.63,-545.72,+368.18,-178.98,-1.22,+0.36,+0.00,+0.00)(7,0)
prout(4)=(+278.76,-114.65,+455.83,+46.98,-86.24,+109.99,+0.00,+0.00)(6,0)
prout(5)=(+278.76,-114.65,+455.83,+46.98,-86.24,+109.99,+0.00,+0.00)(6,0)
prout(6)=(+132.64,-209.48,+462.04,+28.25,-87.85,+92.61,+0.00,+0.00)(6,0)
prout(7)=(+132.64,-209.48,+462.04,+28.25,-87.85,+92.61,+0.00,+0.00)(6,0)
prout(8)=(+179.33,+351.35,+547.26,-101.40,+39.06,-63.18,+0.00,+0.00)(6,1048576)
prout(9)=(+179.33,+351.35,+547.26,-101.40,+39.06,-63.18,+0.00,+0.00)(6,1048576)
prout(10)=(+388.83,+141.32,+561.07,-103.77,+36.76,-109.64,+0.00,+0.00)(6,1048576)
prout(11)=(+388.83,+141.32,+561.07,-103.77,+36.76,-109.64,+0.00,+0.00)(6,1048576)
prout(12)=(-63.07,+434.20,+557.40,-179.97,+3.68,-94.73,+0.00,+0.00)(6,1048576)
prout(13)=(-37.39,+296.00,+777.04,-179.96,+34.65,-94.70,+0.00,+0.00)(6,1048576)
prout(14)=(+321.77,-49.22,+488.13,+43.27,-85.79,+128.35,+0.00,+0.00)(6,0)
prout(15)=(-66.60,+390.87,+454.40,-179.97,+3.68,-94.73,+0.00,+0.00)(6,1048576)
PHome=(+1.27,-251.85,+451.89,+178.98,-90.00,-88.98)(6,0)
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
P_lakkozo2=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
Plakkozo2=(+533.22,+112.48,+532.47,-103.77,+36.76,-109.64)(6,1048576)
P_szalag=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
Pszalag=(-66.60,+390.87,+454.40,-179.97,+3.68,-94.73)(6,1048576)
P_szalag_felett=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
Pszalag_felett=(-37.39,+296.00,+777.04,-179.96,+34.65,-94.70)(6,1048576)
P_hajl1_elott=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
PSzor=(+0.00,+0.00,-150.00,+0.00,+0.00,+0.00,+0.00,+0.00)(6,0)
P_hajl2_elott=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
P_lakkozo1_elott=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
P_lakkozo2_elott=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
PEscape=(-11.31,-50.00,-8.06,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PDestination=(+371.22,-67.94,+535.88,+98.58,+82.02,+63.58,+0.00,+0.00)(6,0)
ptmp=(+179.33,+351.35,+547.26,-101.40,+39.06,-63.18,+0.00,+0.00)(6,1048576)
Pfeszek_felett=(-29.63,-545.72,+368.18,-178.98,-1.22,+0.36)(7,0)
Plakkozo1_elott=(+158.76,+325.14,+548.35,-99.67,+39.92,-60.39)(6,1048576)
Plakkozo2_elott=(+537.06,+117.97,+536.78,-106.37,+35.81,-114.04)(6,0)
Phajl2_elott=(+157.12,-256.81,+549.46,+173.43,-77.12,-50.54)(6,1048576)
Phajl1_elott=(+288.70,-117.52,+546.03,-178.84,-79.66,-23.49)(6,0)
Plakkozok_kozott=(+237.98,-185.15,+508.35,-84.61,+40.79,-136.87)(6,1048576)
Phajl2_2_vez1=(+201.56,-324.08,+513.71,+2.33,-64.27,+118.65)(6,0)
Phajl1_2_vez1=(+403.11,-166.09,+501.05,+8.32,-70.29,+148.92)(6,0)
Phajl2_2_vez2=(+204.15,-330.73,+450.00,+4.35,-76.49,+116.52)(6,0)
Phajl1_2_vez2=(+416.57,-173.30,+460.98,+41.34,-85.75,+115.51)(6,0)
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
P_hajl2=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
P_1=(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)(,)
Plakkozo=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
Jmargit=(-89.56,-22.11,+153.77,+0.66,-41.66,+179.51)
