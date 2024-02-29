1 REM *** THE VALLEY ***
2 REM Agon Light BBCBASIC conversion by JH 2024
3 REM Originally from Computing Today, April 1982
4 REM LLISTed from the Spectravideo 328 version
5 REM Note: BBCBASIC RND(n) returns a number between 1 and n (inclusive)
6 REM Note: BBCBASIC RND(1) returns a float between 0.0 and 0.99999999
7 REM DONE - Replace all PRINT TAB(0,21) with new PROC_statusPrint(msg$)
8 REM DONE - Replace all GOSUB 16000 with new PROC_delayUpdate(delay%, flags$)
9 REM DONE - Move DATA to line numbers less than 32767   (BBCBASIC RESTORE limitation)
10 REM Custom characters added (UDG's):
11 REM - Border (char 128) (was 214 in original)
12 REM - Player (char 129 to 134, for the 5 character classes, + swimming player) (81 in orig) - replaces vpoke 231
13 REM - Path diagonals (char 135 and 136) (was 77 and 78 in orig)
14 REM - Woods (char 137)  (was 216 in orig)
15 REM - Swamp (char 138)  (was 173 in Originally)
16 REM - Castle (the lair in the woods and the swamp) (char 139) (was 230 in orig)
17 REM - The Black Tower (char 140) (was 87 in orig)
18 REM - Doorway in tower (char 141) (was 104 in orig)
19 REM - Woods / Swamp border (char 142) (was 96 in orig)
20 REM - Safe Castle (char 143) (was 219 in orig)
21 REM - Tree (char 144) (was 88 in orig)
22 REM - Water (char 145) (was 224 in orig)
23 REM - Tufts in swamp(char 146) (was 45 in orig)
24 REM - Tower border/walls (char 147) (was 160 in orig)
25 REM - Tower stairs (char 148) (was 102 in orig)
26 REM - Treasure (char 149) (was 42 in orig)
27 REM NOTE - VPEEK and VPOKE do not work with Agon screen/text system, so we have to 
28 REM        keep a "shadow" map of screen chars (see MAP%() below)
39 REM Main program starts here
40 PRINT RND(-TIME)
50 MODE 9 : REM 320 x 240 X 16
51 COLOUR 15   : REM White
90 REM *** Define major vars
91 REM D() is used when drawing vertical walls in the lair/castle
92 REM G() is used for path and safe castle posns and charvals (even = pos, odd = char)
93 REM P() is used for patterns of rooms on diff levels of lair/tower
94 REM N() random nums between 4 and 8 (but not 5), determines depths of rooms in lairs
95 REM S() is positions of the various scanrios in the valley
96 REM T() is counts of the 3 treaure types (T(0) = amulet, T(1) = amulet stones, T(2) = helm of evannah)
97 REM MN$() Monster names (prefixed by scene mask char)
98 REM MS() Monster base strength      MM() Monster base magic power
100 DIM D(3),G(73),P(8),N(8),S(4),T(3),MN$(18),MS(18),MM(18)
105 TM=250 : REM Time delay factor (3 in original listing)
110 LET RATING=0
130 TS=0 : TURNS = 0 : FIGHTING = 0
140 G = 0 : MS = 0 : E = 0
150 HF% = 0 : REM This is used uninitialised in line 3730 !
152 LET Q1% = 0
153 LET MAPWIDTH% = 40 : LET MAPHEIGHT% = 15    : REM Original was 40x14 I think?
154 LET MAPSIZE% = MAPWIDTH% * MAPHEIGHT%
155 DIM MAP%(MAPSIZE%)        : REM Map shadow character buffer 
159 REM const var names for various characters
160 BORDERCHAR% = 128 : PLAYERCHAR% = 129 : PATH1CHAR% = 135 : PATH2CHAR% = 136
162 WOODSCHAR% = 137 : SWAMPCHAR% = 138 : LAIRCHAR% = 139 : towerchar% = 140
164 DOORCHAR% = 141 : WOODSBORDERCHAR% = 142 : SAFECHAR% = 143 : TREECHAR% = 144
166 WATERCHAR% = 145 : TUFTCHAR% = 146 : WALLCHAR% = 147 : STAIRCHAR% = 148 : TREASURECHAR% = 149
299 REM Skip scene data and load monster data
300 RESTORE 30010
310 FOR I% = 0 TO 18 : READ MN$(I%),MS(I%),MM(I%) : NEXT I%
399 :
400 REM Load custom redefined characters ("UDG")
410 PROC_defineCustomChars
498 :
499 REM Main menu screen
500 CLS : PROC_disableCursor
501 PRINT "          ***  THE VALLEY  ***"
502 PRINT : PRINT
503 PRINT : PRINT "         [N]ew Quest"
504 PRINT : PRINT "         [L]oad Saved Quest"
506 PRINT : PRINT "         [I]nstructions"
507 PRINT TAB(6, 23) "Agon Light version by Jum Hig"
508 PRINT TAB(9, 11) "Which ?";
520 GC$ = FN_getFilteredKey("NnLlIi") : REM ** Uniget routine
530 IF GC$ = "N" OR GC$ = "n" THEN 1200
540 IF GC$ = "L" OR GC$ = "l" THEN 1000
599 REM ...else show instructions
600 CLS : PRINT "INSTRUCTIONS:"
603 PRINT
605 PRINT "Heed the wizard Alarian's request to"
606 PRINT "enter The Valley and retrieve the "
607 PRINT "missing Helm of Evanna."
608 PRINT
610 PRINT "'I cannot promise you riches, though "
615 PRINT "there are treasures in the Valley."
620 PRINT
625 PRINT "I ask only that you save the kingdom..."
630 PRINT
635 PRINT "Find the Amulet. Fill it with the 6 "
640 PRINT "gemstones and you will have the ability"
645 PRINT "to cheat death."
650 PRINT
655 PRINT "You will need it to find the Helm of"
660 PRINT "Evanna in the Dark Lord's lair. The "
665 PRINT "Helm will also not reveal itself to "
670 PRINT "anyone with at least the power and "
675 PRINT "experience of a Warlord...'"
680 PRINT : PRINT
685 PROC_pressAnyKey    : REM Press any key routine
699 REM Movement (keys) instructions
700 CLS : PRINT "MOVEMENT KEYS:"
705 PRINT
710 PRINT "               8"
711 PRINT "            7     9 " : PRINT
712 PRINT "           4   *   6" : PRINT
713 PRINT "            1     3"
714 PRINT "               2"
715 PRINT
716 PRINT "Use number keys to move in the eight"
717 PRINT "directions above."
718 PRINT
720 PRINT "COMBAT KEYS:"
721 PRINT "       H to hit monster's head"
722 PRINT "       B to hit monster's body"
723 PRINT "       L to hit monster's legs"
724 PRINT "       S to cast a spell"
725 PRINT : PRINT
730 PROC_pressAnyKey    : REM Press any key routine
749 REM Spells instructions
750 CLS : PRINT "SPELLS:"
751 PRINT
755 PRINT "  1 - 'SLEEP'"
756 PRINT "         - May put enemy to sleep"
757 PRINT "         - Drains 5 stamina"
758 PRINT
760 PRINT "  2 - 'PSI-LANCE'"
761 PRINT "         - Requires 1000 exp and 49 "
762 PRINT "           psi power."
763 PRINT "         - Good against monsters with"
764 PRINT "           magic strength"
765 PRINT "         - Uses 10 stamina"
766 PRINT
770 PRINT "  3 - 'CRISPIT'"
771 PRINT "         - Casts a fearsome bolt of"
772 PRINT "           lightning"
773 PRINT "         - Needs 5000 exp and 77 psi" 
774 PRINT "           power"
775 PRINT "         - Uses 20 stamina"
776 PRINT : PRINT
780 PROC_pressAnyKey    : REM Press any key routine
790 GOTO 500
998 :
999 REM ** Load character save
1000 CLS
1029 REM ** Load character from tape as per original listing line 1000 on
1030 FH = OPENIN "VALLEY.SAV" : IF FH = 0 THEN 1200
1040 INPUT#FH, NAME$, CLASS$, TS, EX, TURNS, CS, PS, T(0), T(1), T(2), P1, C1
1050 CLOSE#FH
1060 STAMINA = 150
1070 IF NAME$ <> "" THEN PRINT "Character state loaded" : GOTO 1400 : REM Start game with loaded stats
1198 :
1199 REM ** Input character name
1200 CLS : COLOUR 15 : REM White
1205 PROC_enableCursor : INPUT "ENTER YOUR NAME: ";NAME$
1210 IF NAME$="" THEN GOTO 1200
1230 PRINT
1240 PRINT "CHOOSE A CHARACTER TYPE WISELY:" : PRINT
1260 PRINT "           WIZARD     [1]"
1261 PRINT "           THINKER    [2]"
1262 PRINT "           BARBARIAN  [3]"
1263 PRINT "           WARRIOR    [4]"
1264 PRINT "           CLERIC     [5]"
1265 PRINT : PRINT "PRESS KEY 1-5 ";
1310 GC$ = INKEY$(10) : IF GC$ = "" THEN GOTO 1310
1312 PROC_disableCursor
1315 A = VAL(GC$)
1320 REM What does this do? -  LOCATE,,0
1330 IF A=1 THEN CLASS$="WIZARD":P1=2:C1=.5:CS=22:PS=28
1340 IF A=2 THEN CLASS$="THINKER":P1=1.5:C1=.75:CS=24:PS=26
1350 IF A=3 THEN CLASS$="BARBARIAN":P1=.5:C1=2:CS=28:PS=22
1360 IF A=4 THEN CLASS$="WARRIOR":P1=1:C1=1.25:CS=26:PS=24
1370 IF A=5 THEN CLASS$="CLERIC":P1=1.25:C1=1:CS=20:PS=20
1380 IF A<1 OR A>5 THEN CLASS$="DOLT":P1=1:C1=1:CS=20:PS=20
1390 EX=5 : STAMINA=150
1400 PRINT : PRINT : PRINT"GOOD LUCK, ";NAME$;" THE ";CLASS$
1420 PROC_delayUpdate(150, "D") : REM Delay and update status routine
1430 GOSUB 10000 : REM Draw The Valley scene
1440 PROC_delayUpdate(5, "")
1450 GOTO 2000
1499 REM *** Player input (UNIGET ROUTINE)
1500 DEF FN_getFilteredKey(keyMask$)
1501    PROC_enableCursor
1502    GC$ = INKEY$(10) : IF GC$ = "" THEN GOTO 1502
1505    A = FALSE    : REM Found flag (neccessary as cannot return from inside loop)
1510    FOR I% = 1 TO LEN(keyMask$)
1520        IF MID$(keyMask$,I%,1)=GC$ THEN A = TRUE
1530    NEXT I%
1540    IF A = FALSE THEN GOTO 1502	: REM Not a valid input - try again
1545    PROC_disableCursor
1550 = GC$
1599 REM *** ANYKEY ROUTINE (used when loading and saving state)
1600 DEF PROC_pressAnyKey
1605    PRINT "** PRESS ANY KEY TO CONTINUE **"
1610    GC$=INKEY$(10) : IF GC$ = "" THEN GOTO 1610
1620 ENDPROC
1699 REM *** Timed player input (COMBAT GET ROUTINE)
1700 DEF PROC_timedInput
1705    GC$=INKEY$(1)  : REM clear keyboard buffer
1710    TV%=0 : T=TIME : PROC_enableCursor
1720    GC$=INKEY$(1) : IF GC$ = "" AND (TIME - T) < 500 THEN GOTO 1720
1725    PROC_disableCursor
1759    REM - For Agon, I think time is in 100ths of a second?
1760    IF TIME-T > 500 THEN TV%=1 : REM key not pressed on time (was dt > 200 in original?)
1770    PROC_statusClear : REM was PRINT TAB(0,21) SPC(36) : REM Wipe message line
1780 ENDPROC
1799 REM *** Get restore colour for various objects when player moves off them
1800 DEF FN_getItemColour(itemChar%)
1805    LOCAL IC%
1810    IC% = 15   : REM default to WHITE
1815    IF itemChar% = PATH1CHAR% OR itemChar% = PATH2CHAR% THEN IC% = 7
1830    IF itemChar% = LAIRCHAR% THEN IC% = 15
1835    IF itemChar% = towerchar% THEN IC% = 15
1840    IF itemChar% = SAFECHAR% THEN IC% = 11
1842    IF itemChar% = WOODSCHAR% OR itemChar% = TREECHAR% OR itemChar% = TUFTCHAR% THEN IC% = 10
1845    IF itemChar% = SWAMPCHAR% THEN IC% = 5
1850    IF itemChar% = WATERCHAR% THEN IC% = 4
1855   REM          WHEN STAIRCHAR% : IC% = 7
1890 =IC%
1999 REM *** Player movement
2000 M=W : PK=FN_vpeek(W) : COLOUR 11 : PROC_vpoke(M, PLAYERCHAR%)  : REM draw player at new pos, remembering what was there
2010 STAMINA = STAMINA + 10
2020 IF PK=PATH1CHAR% OR PK=PATH2CHAR% THEN 2040 : REM Path char
2030 PRINT TAB(0,22) "WHICH DIRECTION ?"; : GOTO 2050
2040 PRINT TAB(0,22) "SAFE ON THE PATH...WHICH WAY ?";
2050 GC$=INKEY$(1) : PROC_enableCursor : REM clear any buffered keystroke
2060 GC$=INKEY$(10) : IF GC$ = "" THEN GOTO 2060
2062 PROC_disableCursor
2065 IF GC$="I" OR GC$="i" THEN PROC_showRating : GOTO 2030   : REM JH - press 'I' to show rating
2069 REM Special routine for use with numeric keypads
2070 A=VAL(GC$) : IF A=0 THEN 2060
2080 IF A>3 THEN A=A-3 : GOTO 2080
2090 W=M+A-2-MAPWIDTH%*(INT((VAL(GC$)-1)/3)-1)
2100 TURNS=TURNS+1 : PROC_statusClear : REM was PRINT TAB(0,21);SPC(36);
2109 REM Am I stepping on something?
2110 Q=PLAYERCHAR% : Q1%=FN_vpeek(W) : IF Q1%=32 OR Q1%=TUFTCHAR% THEN 2190  : REM Stepping on npthing, or tufts (45 in orig)
2105 REM PRINT TAB(1,1);
2106 REM PRINT W;" ";Q1%
2120 IF Q1%=SAFECHAR% THEN GOTO 20000 : REM was GOTO 48000 : REM Quit (stepped on safe castle)
2130 IF Q1%=BORDERCHAR% OR Q1%=TREECHAR% OR Q1%=WALLCHAR% THEN TURNS=TURNS-1 : GOTO 2030 : REM Hit border, wall or tree
2140 IF Q1%=WOODSCHAR% OR Q1%=SWAMPCHAR% OR Q1%=towerchar% OR Q1%=LAIRCHAR% THEN GOTO 9000 : REM Scene entry
2150 IF Q1%=DOORCHAR% OR Q1%=WOODSBORDERCHAR% THEN 9090 : REM Scene exit   (was 104 and 96 in original)
2160 IF Q1%=STAIRCHAR% THEN GOTO 15000 : REM Stairs
2170 IF Q1%=WATERCHAR% OR (GC$="5" AND PK=WATERCHAR%) THEN Q=134 : STAMINA=STAMINA-20 : IF STAMINA<=0 THEN 23000 : REM Player in water
2180 IF Q1%=TREASURECHAR% THEN GOTO 2800 : REM Special find / treasure
2190 COLOUR FN_getItemColour(PK) : PROC_vpoke(M, PK)   : REM Restore char we were standing on
2195 PK=FN_vpeek(W) : M=W : COLOUR 11 : PROC_vpoke(M, Q)  : REM Save char at pos we are now moving to
2200 IF PK=PATH1CHAR% OR PK=PATH2CHAR% THEN PROC_delayUpdate(5, "") : GOTO 2260 : REM path char
2209 REM * Nothing, monster or special find?
2210 RF = RND(100) : REM was RND(TIME), then compared 0.2 and 0.7
2220 IF RF < 20 THEN 3000 : REM Monster!
2230 IF RF > 70 THEN 2300 : REM Find select
2240 PROC_statusPrint("NOTHING OF VALUE...")
2250 PROC_delayUpdate(50, "")
2260 GOTO 2010
2299 REM *** Choose random "element" for current location
2300 RF = INT(RND(6))  : REM was RND(TIME)*6+1
2305 PRINT TAB(0,22) "";
2310 ON RF GOSUB 2340,2380,2380,2410,2410,2440
2320 PROC_delayUpdate(80, "")
2330 GOTO 2010
2340 PRINT"A CIRCLE OF EVIL...DEPART IN HASTE !"
2350 CS=CS+INT((FL+1)/2):PS=PS-INT((FL+1)/2):STAMINA=STAMINA-20
2360 IF STAMINA <= 0 THEN 23000 : REM Death
2370 RETURN
2380 PRINT"A HOARD OF GOLD"
2390 TS=TS+INT(FL*RND(100)+100) : RETURN   : REM Was RND(TIME)*100+100
2410 PRINT"YOU FEEL THE AURA OF DEEP MAGIC..."
2420 PRINT TAB(8);"...ALL AROUND YOU...":GOTO2450
2440 PRINT"...A PLACE OF ANCIENT POWER..."
2450 PS=PS+2+INT(FL*P1):CS=CS+1+INT(FL*C1):STAMINA=STAMINA+25:RETURN
2799 REM *** Special finds routine
2800 PROC_vpoke(M, 32) : M=W : PK=32 : PROC_vpoke(M, PLAYERCHAR%)  : REM was 231
2810 RN=RND(100) : PROC_statusClear : PRINT TAB(0,22) ""; : REM was RND(TIME)
2820 IF S=6 AND RN>=95 AND T(1)=6 AND T(2)=0 AND RATING>25 THEN T(2)=1 : GOTO2870
2830 IF S=5 AND RN>=85 AND T(0)=0 THEN T(0)=1 : GOTO2880
2840 IF S=4 AND RN>=70 AND T(0)=1 AND T(1)<6 AND FL>T(1) THEN 2890
2850 IF RN>=43 THEN PRINT"A WORTHLESS BAUBLE" : GOTO2940
2860 PRINT"A PRECIOUS STONE !":GOTO2930
2870 PRINT"YOU FIND THE HELM OF EVANNA !":GOTO2930
2880 PRINT"THE AMULET OF ALARIAN...EMPTY...":GOTO2930
2890 PRINT"AN AMULET STONE..."
2900 PROC_delayUpdate(60, "D")
2910 IF RN>=85 THEN PRINT TAB(15);"...BUT THE WRONG ONE !":GOTO2940
2920 PRINT TAB(15);"...THE STONE FITS !":T(1)=T(1)+1
2930 TS=TS+100*(T(0)+T(1)+T(2)+FL)
2940 PROC_delayUpdate(80, "")
2950 GOTO 2010 
2999 REM *** Monster select routine
3000 PROC_statusPrint("OH OH...") : MS=0:MM=0:FIGHTING=1 : REM Was "BEWARE... THOUS HAST ENCOUNTERED"
3020 RF=INT(RND(17)) : IF RF>9 AND RND(100) >= 85 THEN 3020 : REM Was RND(TIME)*17 and RND(TIME)
3030 IF Q1%=WATERCHAR% OR PK=WATERCHAR% THEN RF=INT(RND(2)+17) : REM Was INT(RND(TIME)*2+17
3040 IF RF=16 AND RND(100) < 70 THEN 3020
3050 IF FL<5 AND RF=15 THEN 3020
3060 X$=LEFT$(MN$(RF),1)
3070 FOR I% = 1 TO LEN(F$)
3080 IF MID$(F$,I%,1)=X$ THEN 3110
3090 NEXT I% : GOTO3020
3110 M$=RIGHT$(MN$(RF),LEN(MN$(RF))-1)
3120 IF MS(RF)=0 THEN 3150
3130 MS=INT((CS*.3)+MS(RF)*FL^.2/(RND(2))) : REM Was RND(TIME)+1
3140 IF MM(RF)=0 THEN 3160
3150 MM=INT(MM(RF)*FL^.2/(RND(2))) : REM Was RND(TIME)+1
3160 U=INT((RF+1)*(FL^1.5))
3170 IF RF>23 THEN U=INT((RF-22)*FL^1.5)
3175 IF RND(100)>40 THEN 3185
3180 PRINT TAB(12-LEN(M$)/2);"AN EVIL ";
3185 PRINT M$;"!"
3190 PROC_delayUpdate(40, "")
3499 REM *** Player combat routine
3500 IF RND(100) < 60 THEN 4000 : REM Monsters combat turn
3510 PROC_statusPrint("YOU SURPRISED IT! ATTACK OR RETREAT? ")
3520 PROC_timedInput : REM Combat get  (was GOSUB 1700) 
3530 IF GC$="R" OR GC$="r" THEN 3900
3540 IF TV%=1 THEN 3600
3550 IF GC$<>"A" AND GC$<>"a" THEN 4000
3560 PROC_delayUpdate(30, "D")
3570 PROC_statusPrint("*** STRIKE QUICKLY *** ")
3572 REM JH - Show combat instructions at bottom of screen
3573 REM PRINT TAB(0,25) "Press H to strike head, B for body, L for legs."
3574 REM PRINT TAB(0,26) "Press S to cast a spell.                       "
3575 REM PRINT TAB(32,27) "  ";
3580 PROC_timedInput : REM Combat get (was GOSUB 1700)
3590 IF TV%=0 THEN 3620 
3600 PROC_statusPrint("* TOO SLOW...TOO SLOW * ")
3610 HF% = 0 : GOTO 3830
3620 E=39*LOG(EX)/3.14
3630 IF GC$="S" OR GC$="s" THEN 4500 : REM Spell control
3640 IF MS=0 THEN PROC_statusPrint("YOUR SWORD AVAILS YOU NOUGHT HERE") : GOTO 3830
3650 STAMINA=STAMINA-1
3660 IF STAMINA<=0 THEN PROC_statusPrint("YOU FATALLY EXHAUST YOURSELF") : GOTO 23000
3665 REM PRINT "CS = ";CS;"   MS = ";MS
3670 RF=RND(10)
3680 IF (GC$="H" OR GC$="h") AND (RF<7 OR CS>MS*4) THEN Z=2 : GOTO 3730
3690 IF (GC$="B" OR GC$="b") AND (RF<9 OR CS>MS*4) THEN Z=1 : GOTO 3730
3700 IF (GC$="L" OR GC$="l") AND (RF<9 OR CS>MS*4) THEN Z=.3 : GOTO 3730
3710 PROC_statusPrint("YOU MISSED IT !")
3720 HF% = 0 : GOTO3830
3730 IF HF%=1 THEN D=MS+INT(RND(9)) : HF%=0 : GOTO3760
3740 D=INT((((CS*RND(50))-(10*MS)+E)/100)*Z) : IF D<0 THEN D=0
3750 IF CS>(MS-D)*4 THEN HF%=1
3760 MS=MS-D
3770 PROC_statusPrint("A HIT... ")
3780 PROC_delayUpdate(60, "D")
3790 IF D=0 THEN PRINT TAB(9,22);"BUT NO DAMAGE":HF%=0:GOTO3830
3800 PRINT TAB(9, 22);D;" DAMAGE..." : IF MS<=0 THEN 3860 : REM It's dead
3810 IF HF%=1 THEN PROC_delayUpdate(30, "D") : PRINT "THE ";M$;" STAGGERS..."
3830 PROC_delayUpdate(110, "")
3840 IF HF%=1 THEN 3570
3850 GOTO 4000 : REM Monster's combat turn
3860 PROC_statusPrint("...KILLING THE MONSTER...")
3870 EX=EX+U : HF%=0 : FIGHTING=0
3880 PROC_delayUpdate(80, "")
3890 GOTO 2010 : REM Player movement
3900 PROC_statusPrint("KNAVISH COWARD !") : FIGHTING=0
3905 EX = EX - (U MOD  20) : IF EX < 0 THEN EX = 0 : REM JH - Added experience penalty for running away :)
3910 GOTO 3880
3999 REM *** Monster's combat routine
4000 PROC_statusPrint("THE CREATURE ATTACKS...")
4010 PROC_delayUpdate(50, "W")
4030 IF MS=0 OR MS<MM AND MM>6 AND RND(100) < 50 THEN 4300
4040 MS=MS-1 : IF MS<=0 THEN 4240
4050 RF=INT(RND(10))
4060 PRINT TAB(0,22) ""; : ON RF GOTO 4070,4080,4090,4100,4110,4110,4120,4120,4130,4140
4070 PRINT"IT SWINGS AT YOU...AND MISSES":GOTO4280
4080 PRINT"YOUR BLADE DEFLECTS THE BLOW":GOTO4280
4090 PRINT"..BUT HESITATES, UNSURE...":GOTO4280
4100 Z=3:PRINT"IT STRIKES YOUR HEAD !":GOTO4150
4110 Z=1.5:PRINT"YOUR CHEST IS STRUCK !":GOTO4150
4120 Z=1:PRINT"A STRIKE TO YOUR SWORD ARM !":GOTO4150
4130 Z=1.3:PRINT"A BLOW TO YOUR BODY !":GOTO4150
4140 Z=.5:PRINT"A BLOW CATCHES YOUR LEGS !"
4150 PROC_delayUpdate(60, "D")
4160 G=INT((((MS*RND(75))-(10*CS)-E)/100)*Z)
4165 PRINT TAB(0,22) ""; 
4170 IF G<0 THEN G=0 : PRINT"...SAVED BY YOUR ARMOUR !  ":GOTO 4280
4180 STAMINA=STAMINA-G
4190 IFG>9 THEN CS=INT(CS-G/6)
4200 IFG=0 THEN PRINT"SHAKEN......BUT NO DAMAGE DONE":GOTO 4280
4210 PRINT "YOU TAKE ";G;" DAMAGE          "
4220 IF CS <= 0 OR STAMINA <= 0 THEN GOTO 23000
4230 GOTO 4280
4240 PROC_statusPrint("...USING ITS LAST ENERGY")
4250 EX=INT(EX+U/2):FIGHTING=0: PROC_delayUpdate(100, ""):GOTO 2010
4280 PROC_delayUpdate(100, "")
4290 GOTO 3570
4300 PROC_statusPrint("...HURLING A LIGHTNING BOLT AT YOU !")
4310 G=INT(((180*RND(MM))-(PS+E))/100) : MM=MM-5 : IF G>9 THEN MM=MM-INT(G/5)
4320 PROC_delayUpdate(80, "W")
4330 IF MM<=0 THEN MM=0 : GOTO 4240
4340 IF RND(100) < 25 THEN 4410
4350 IF G<=0 THEN G=0 : GOTO 4400
4360 PROC_statusPrint("IT STRIKES HOME !") : PROC_delayUpdate(110, "")
4380 STAMINA=STAMINA-G  :IF G>9 THEN PS=INT(PS-G/4)
4390 GOTO 4210
4400 PROC_statusPrint("YOUR PSI SHIELD PROTECTS YOU") : GOTO 4280
4410 PROC_statusPrint("...MISSED YOU !") : GOTO 4280
4499 REM *** Spell control routine
4500 PROC_statusPrint("WHICH SPELL SEEK YE? (PRESS 1, 2 OR 3)")
4505 PROC_timedInput : REM Combat get (was GOSUB 1700)
4510 IF TV%=1 THEN 3600 : REM Too slow
4520 IF VAL(GC$)>0 AND VAL(GC$)<=3 THEN 4540
4530 PROC_statusPrint("NO SUCH SPELL...     ") : GOTO 4640
4540 IF 4*RND(PS)<=MM THEN 4590
4550 PRINT TAB(0,22) "" : ON VAL(GC$) GOSUB 5000,5200,5400
4560 PRINT TAB(0,22) "" : ON SC GOTO 4620,4640,4660,4570,4600,4580,4590
4570 PRINT"IT IS BEYOND YOU!    ":GOTO 4640
4580 PRINT:PRINT"BUT THE SPELL FAILS...!":GOTO 4640
4590 PRINT"NO USE, THE BEASTS PSI SHIELDS IT":GOTO 4640
4600 PRINT"THE SPELL SAPS ALL YOUR STRENGTH": GOTO 23000 : REM Death
4620 PROC_delayUpdate(100, "") : GOTO2010 : REM Movement
4640 PROC_delayUpdate(60, "") : GOTO4000 : REM Monster's combat
4660 PROC_delayUpdate(60, "") : GOTO3570 : REM Player's combat
4999 REM *** Spell 1 (SLEEPIT)
5000 STAMINA=STAMINA-5:IF STAMINA<=0 THEN SC=5 : RETURN
5010 PRINT "SLEEP YOU FOUL FIEND THAT I MAY ESCAPE  AND PRESERVE MY MISERABLE SKIN":DF=180:PROC_delayUpdate(DF, "")
5040 PROC_statusPrint("THE CREATURE STAGGERS...")
5050 PROC_delayUpdate(40, "D") : IF RND(100) < 50 THEN 5090
5070 PRINT "AND COLLAPSES...STUNNED"
5080 EX=INT(EX+U/2):FIGHTING=0:SC=1:RETURN
5090 PRINT "BUT RECOVERS WITH A SNARL !" : SC=2 : RETURN
5199 REM *** Spell 2 (PSI_LANCE)
5200 IF MS>STAMINA OR PS<49 OR EX<1000 THEN SC=4 : RETURN
5210 STAMINA=STAMINA-10 : IF STAMINA<=0 THEN SC=5 : RETURN
5220 IF MM=0 THEN PRINT "THIS BEAST HAS NO PSI TO ATTACK":SC=2:RETURN
5230 PRINT "WITH MY MIND I BATTLE THEE!"
5240 PROC_delayUpdate(120, "")
5250 RF=RND(100) : IF RF<40 AND MM>10 THEN SC=6 : RETURN
5260 D=INT((((CS*50*RF/100)-5*(MS+MM)+E)/50)/4)
5270 IF D<=0 THEN D=0 : SC=7 : RETURN
5280 PRINT TAB(0,22) "THE PSI-LANCE CAUSES ";D*2;" DAMAGE  "
5290 MM=MM-3*D : IF MM<=0 THEN MM=0
5300 MS=MS-D : IF MS<=0 THEN MS=0
5310 IF (MS+MM)>0 THEN SC=2 : RETURN
5320 PRINT "...KILLING THE CREATURE":EX=EX+U:FIGHTING=0:SC=1:RETURN
5399 REM *** Spell 3 (CRISPIT)
5400 IF PS<77 OR EX<5000 THEN SC=4 : RETURN
5410 STAMINA=STAMINA-20 : IF STAMINA<=0 THEN SC=5 : RETURN
5420 PRINT "WITH THE POWER OF MY SPELL I CURSE THEE-BURN YE SPAWN OF HELL AND SUFFER..." : PROC_delayUpdate(240, "")
5460 PROC_statusPrint("A BOLT OF ENERGY LASHES AT THE BEAST...") : PROC_delayUpdate(80, "W")
5480 IF RND(10) > (PS/78)*(5-P1) THEN PROC_statusPrint("CURSES! MISSED IT!") : SC=2 : RETURN
5490 D=INT((CS+RND(PS))-(MM*RND(10)))
5500 IF D<=0 THEN D=0:SC=7 : RETURN
5510 IF MS=0 THEN MM=MM-D : GOTO5530
5520 MS=MS-D : IF D<10 THEN MM=INT(MM-(D/3))
5530 PRINT TAB(0,22) "IT STRIKES HOME, CAUSING ";D;" DAMAGE  "
5540 IF (MS+MM)<=0 THEN 5570
5550 PROC_delayUpdate(80, "D") : SC=2 : RETURN
5570 PRINT "THE BEAST DIES SCREAMING !" : EX=EX+U : FIGHTING=0 : SC=1 : RETURN
8998 :
8999 REM *** Scenario control routine
9000 IF Q1%=LAIRCHAR% AND PK=WATERCHAR% THEN PROC_statusPrint("YOU CANNOT ENTER THIS WAY...") : GOTO9110
9010 FOR I% = 2 TO 7
9020     P(I%)=0
9030     N(I%)=INT(RND(5)+3)  : REM We want 4 to 8, but not 5
9040     IF N(I%)=5 THEN 9030
9050 NEXT I%
9060 IF S=1 THEN MP=M
9070 P(2)=INT(RND(30)) : TF=TURNS : GOTO 9130
9090 IF TURNS>TF+INT(RND(6)) THEN 9130
9100 PROC_statusPrint("THE WAY IS BARRED")
9110 TURNS=TURNS-1 : STAMINA=STAMINA-10 : PROC_delayUpdate(100, "W") : GOTO 2010
9130 STAMINA=STAMINA-10 : PROC_vpoke(M, 32) : PROC_vpoke(W, Q)
9140 IF Q1%=WOODSBORDERCHAR% THEN S=1 : FL=1                   : REM 96 in original (woods/swamp border)
9150 IF Q1%=DOORCHAR% AND S=4 THEN S=1:FL=1            : REM 104 in original (doorway in scene 4, 5, 6)
9160 IF Q1%=DOORCHAR% AND S=5 OR S=6 THEN S=S-3:FL=FL-4:M=MW
9170 IF Q1%=SWAMPCHAR% THEN S=2:FL=2                    : REM 173 in original (swamps)
9180 IF Q1%=WOODSCHAR% THEN S=3:FL=3                     : REM 216 in oriinal (woods)
9190 IF Q1%=WOODSCHAR% OR Q1%=SWAMPCHAR% THEN D2=RND(8) : R2=P(2)
9200 IF Q1%=towerchar% THEN S=4:FL=2                      : REM 87 in original (Black Tower)
9210 IF Q1%=LAIRCHAR% THEN S=S+3:FL=FL+4:MW=M          : REM 230 in original (woods/swamp "lair")
9220 ON S GOSUB 10000,12000,12010,14000,14010,14010
9240 PROC_delayUpdate(5, "")
9250 GOTO 2000	: REM Player movement
9998 :
9999 REM *** Draw Scene 1 (The Valley)
10000 CLS : PROC_disableCursor : PROC_clearMap
10005 F$="VAEGH" : FL=1 : S=1
10006 COLOUR 7    : REM grey for mountain "frame"
10010 FOR I% = 0 TO MAPWIDTH% - 1 : PROC_vpoke(I%, BORDERCHAR%) : NEXT I% : REM Top border
10020 FOR I% = 1 TO MAPHEIGHT% - 2
10025    PROC_vpoke(I% * MAPWIDTH%, BORDERCHAR%)
10030    PROC_vpoke(I% * MAPWIDTH% + 39, BORDERCHAR%)
10040 NEXT I% 
10050 FOR I% = 0 TO MAPWIDTH% - 1 : PROC_vpoke(MAPSIZE% - MAPWIDTH% + I%, BORDERCHAR%) : NEXT I% : REM Bottom border
10060 IF G(0)<>0 THEN 10190
10069 REM First draw - Place safe castle
10070 M = MAPWIDTH% + 1 + INT(RND(11)) * MAPWIDTH%
10080 L=M  :MP=M : W=M : G(0)=M : G(1)=SAFECHAR%
10088 REM First draw - Generate path
10090 FOR I% = 2 TO 72 STEP 2
10100     IF RND(100) > 50 THEN 10120
10110     PC = PATH1CHAR% : L1 = L + MAPWIDTH%+1 : GOTO 10130     : REM was 77
10120     PC = PATH2CHAR% : L1 = L - MAPWIDTH%+1                  : REM was 78
10130     IF L1 >= MAPSIZE% - MAPWIDTH% OR L1 < MAPWIDTH% THEN 10100   : REM TODO - Check with new map height
10140     G(I%+1)=PC
10150     IF I%>2 AND G(I%+1)<>G(I%-1) THEN L1=L+1
10160     G(I%)=L1 : L=L1 : PROC_vpoke(G(I%), G(I%+1))
10170 NEXT I%
10180 G(73)=SAFECHAR%
10189 REM Plot path
10190 FOR I% =0 TO 72 STEP2
10200     PROC_vpoke(G(I%), G(I%+1))
10210 NEXT I%
10220 IF S(0)<>0 THEN 10280
10229 REM First draw - Compute random scenario positions
10230 FOR I% = 0 TO 4
10240     Y%=INT(RND(12)) : X%=INT(RND(34))     : REM  X% range 1 to 34
10250     S(I%) = MAPWIDTH% + 1 + (Y% * MAPWIDTH%) + X%
10260     IF FN_vpeek(S(I%)) <> 32 OR FN_vpeek(S(I%)+1) <> 32 THEN 10240
10270 NEXT I%
10278 REM Draw woods, swamps and tower markers
10279 COLOUR FN_getItemColour(WOODSCHAR%)  : REM Bright green
10280 PROC_vpoke(S(0), WOODSCHAR%) : PROC_vpoke(S(0)+1, WOODSCHAR%) : PROC_vpoke(S(1), WOODSCHAR%) : PROC_vpoke(S(1)+1, WOODSCHAR%) : REM Woods (was 216)
10290 PROC_vpoke(S(2), SWAMPCHAR%) : PROC_vpoke(S(2)+1, SWAMPCHAR%) : PROC_vpoke(S(3), SWAMPCHAR%) : PROC_vpoke(S(3)+1, SWAMPCHAR%) : REM Swamps (was 173)
10295 COLOUR FN_getItemColour(towerchar%)  : REM White
10300 PROC_vpoke(S(4), towerchar%) : REM Black Tower (was 87)
10310 M=MP:W=M
10330 RETURN
11998 :
11999 REM *** Scenario 2 (Woods and swamps)
12000 F$="AFL" : PC=TUFTCHAR% : GOTO 12020
12010 F$="FAEHL" : PC=TREECHAR% : PK=32
12020 CLS : PROC_disableCursor : PROC_clearMap
12025 COLOUR FN_getItemColour(TREECHAR%)   : REM Brigth green
12030 FOR I% = 1 TO 200 : PROC_vpoke(RND(MAPSIZE% - 1), PC) : NEXT I%  : REM Plot trees or tufts
12077 REM Plot lake
12078 COLOUR FN_getItemColour(WATERCHAR%) : REM Dark blue
12079 A% = WATERCHAR%
12080 PROC_vpokeXY(R2+2, D2, A%) : PROC_vpokeXY(R2+3, D2, A%)
12081 PROC_vpokeXY(R2+1, D2+1, A%) : PROC_vpokeXY(R2+2, D2+1, A%) : PROC_vpokeXY(R2+3, D2+1, A%) : PROC_vpokeXY(R2+4, D2+1, A%) : PROC_vpokeXY(R2+5, D2+1, A%)
12082 PROC_vpokeXY(R2  , D2+2, A%) : PROC_vpokeXY(R2+1, D2+2, A%) : PROC_vpokeXY(R2+2, D2+2, 32) : PROC_vpokeXY(R2+3, D2+2, 32) : PROC_vpokeXY(R2+4, D2+2, A%) : PROC_vpokeXY(R2+5, D2+2, A%) : PROC_vpokeXY(R2+6, D2+2, A%)
12083 PROC_vpokeXY(R2  , D2+3, A%) : PROC_vpokeXY(R2+1, D2+3, A%) : PROC_vpokeXY(R2+3, D2+3, 32) : PROC_vpokeXY(R2+4, D2+3, A%) : PROC_vpokeXY(R2+5, D2+3, A%) : PROC_vpokeXY(R2+6, D2+3, A%)
12084 PROC_vpokeXY(R2+1, D2+4, A%) : PROC_vpokeXY(R2+2, D2+4, A%) : PROC_vpokeXY(R2+3, D2+4, A%) : PROC_vpokeXY(R2+4, D2+4, A%) : PROC_vpokeXY(R2+5, D2+4, 32) : PROC_vpokeXY(R2+6, D2+4, A%) : PROC_vpokeXY(R2+7, D2+4, A%)
12085 PROC_vpokeXY(R2+3, D2+5, A%) : PROC_vpokeXY(R2+4, D2+5, A%) 
12086 PROC_vpokeXY(R2+4, D2+6, A%)
12088 COLOUR FN_getItemColour(LAIRCHAR%) : PROC_vpokeXY(R2+2, D2+3, LAIRCHAR%)
12148 REM Plot frame
12149 COLOUR 10     : REM BRight green
12150 FOR I% = 0 TO MAPWIDTH% - 1
12155     PROC_vpoke(I%, WOODSBORDERCHAR%)     : REM frame top
12160     PROC_vpoke(MAPSIZE% - MAPWIDTH% + I%, WOODSBORDERCHAR%)   : REM frame bottom
12165 NEXT I%
12170 FOR I% = 1 TO MAPHEIGHT% - 2
12175    PROC_vpoke(I% * MAPWIDTH%, WOODSBORDERCHAR%)   : REM frame left
12180    PROC_vpoke(I% * MAPWIDTH% + (MAPWIDTH% - 1), WOODSBORDERCHAR%) : REM frame right
12185 NEXT I%
12190 COLOUR 15  : REM White
12200 W = MAPSIZE% - MAPWIDTH% - (MAPWIDTH% / 2) : PROC_vpoke(W, 32)         : REM Make sure player appears on a space char
12210 IF Q1%=DOORCHAR% THEN M=MW : W=M  : REM Did player exit from lair to woods/swamp?
12220 RETURN
13998 :
13999 REM *** Scenario 3 (Castle/tower)
14000 F$="CAGE" : P=0 : H=N(FL) : PK=32 : GOTO14020
14010 F$="CBE" : P=0 : H=N(FL) : PK=32 : P(FL)=P(2)
14020 CLS : PROC_disableCursor : PROC_clearMap
14022 COLOUR 7 :   REM Grey
14025 FOR I% = 2 TO 22 : PROC_vpokeXY(I%, 0, WALLCHAR%) : PROC_vpokeXY(I%, 14, WALLCHAR%) : NEXT I%
14030 FOR I% = 1 TO 13 : PROC_vpokeXY(2, I%, WALLCHAR%) : PROC_vpokeXY(22, I%, WALLCHAR%) : NEXT I%
14069 REM ** Draw vertical walls
14070 RESTORE : FOR I% =1 TO P(FL)
14080    READ V : IF V=100 THEN RESTORE
14090 NEXT I%
14100 L1 = MAPWIDTH% + 2
14110 FOR J% =1 TO 3
14120     READ D(J%) : P=P+1
14130     IF D(J%)=100 THEN RESTORE : D(J%)=3 : P=P+1
14140 NEXT J%
14150 FOR I% = 0 TO H : PC=WALLCHAR%
14160     L=L1+(MAPWIDTH%*I%) : IF L>522 THEN I% = 100 : GOTO 14240    : REM JH was GOTO 14260 but causes a REURN without GOSUB stack error
14170     IF I%=1 THEN PC=32
14180     IF D(1)=0 THEN PC=WALLCHAR% : GOTO14200
14190     PROC_vpoke(L+D(1), PC) : PC=WALLCHAR%
14200     IF I%=3 THEN PC=32
14210     PROC_vpoke(L+D(1)+D(2), PC) : PC=WALLCHAR%
14220     IF I%=4 THEN PC=32
14230     PROC_vpoke(L+D(1)+D(2)+D(3), PC) : PC=WALLCHAR%
14240 NEXT I%
14245 IF I% = 100 OR I% = 101 THEN 14260
14250 L1=L1+(MAPWIDTH%*H)+MAPWIDTH% : GOTO 14110
14259 REM ** Draw horiz walls
14260 L1 = MAPWIDTH% + 2
14270 FOR J% =1 TO 4
14280     L=L1+(MAPWIDTH%*J%*(H+1))
14290     FOR K% = 1 TO 19
14300         IF L>482 THEN K% = 100 : GOTO 14330   : REM exit loops  (was GOTO 14350)
14310         PROC_vpoke(L+K%, PC) : IF K%=2 OR K%=3*H OR K%=17 THEN PROC_vpoke(L+K%, 32) : PROC_vpoke(L+K%-MAPWIDTH%, 32) : PROC_vpoke(L+K%+MAPWIDTH%, 32)
14330     NEXT K%
14335     IF K% = 100 OR K% = 101 THEN J% = 100  : REM exit loop
14340 NEXT J%
14348 REM Draw stairs
14349 COLOUR 15  : REM White
14350 IF S=5 OR S=6 THEN 14380
14360 IF FL/2=INT(FL/2) THEN PROC_vpoke(MAPSIZE% - MAPWIDTH% * 2 + 3, STAIRCHAR%) : GOTO14380
14370 PROC_vpoke(61, STAIRCHAR%)
14379 REM Need doorway?
14380 IF FL=2 OR S=5 OR S=6 THEN PROC_vpoke(MAPSIZE% - MAPWIDTH% + 8, DOORCHAR%) : PROC_vpoke(MAPSIZE% - MAPWIDTH% * 2 + 8, 32)
14390 IF P(3)=0 THEN W=MAPSIZE% - MAPWIDTH% * 2 + 8   : REM place player above the doorway    
14398 REM Write appropriate name
14399 COLOUR 11 : REM Yellow
14400 IF S=5 THEN 14470
14410 IF S=6 THEN 14450
14420 PRINT TAB(24,4) "THE BLACK TOWER" : PRINT TAB(27,5) "OF ZAEXON" : PRINT TAB(24,8) "FLOOR ";FL-1 : GOTO14490
14450 PRINT TAB(26,2) " VOUNIM'S " : PRINT TAB(26,3) "   LAIR   "; : GOTO14500
14470 PRINT TAB(25,2) "THE TEMPLE OF" : PRINT TAB(25,3) "  Y'NAGIOTH  ";
14490 P(FL+1)=P(FL)+P
14499 REM Scatter special finds
14500 IF FL<4 OR RND(100) < 30 THEN 14570    : REM return
14505 COLOUR 11 :  REM Yellow 
14510 FOR I% = 1 TO INT(RND(5))+2
14520     N1=INT(RND(19)) : N2=INT(RND(12)) : IF FN_vpeek(43+MAPWIDTH%*N2+N1) <> 32 THEN 14520
14550     PROC_vpoke(43 + MAPWIDTH%*N2 + N1, TREASURECHAR%)
14560 NEXT I%
14570 RETURN
14999 REM *** Stairs routine
15000 COLOUR 15   : REM white
15005 PROC_vpoke(W, PLAYERCHAR%) : PROC_vpoke(M, 32)  : REM was 231
15010 PROC_statusPrint("A STAIRWAY...UP OR DOWN ?") : TV%=FL
15020 GC$ = FN_getFilteredKey("UDud") : IF GC$="U" OR GC$="u" THEN FL=FL+1 : GOTO 15050
15040 FL=FL-1
15050 IF FL>7 OR FL<2 THEN 15080
15060 PROC_delayUpdate(25, "D")
15070 GOTO 9220
15080 PROC_statusPrint("THESE STAIRS ARE BLOCKED ") : PROC_delayUpdate(60, "D") : FL=TV%
15090 GOTO 15010
15998 :
15999 REM *** DELAY/WIPE/UPDATE ROUTINE  (PROC VERSION)
16000 DEF PROC_delayUpdate(delay%, flags$)
16001     PROC_disableCursor
16002     REM *DELAY*
16005     FOR DL = 1 TO (delay% * TM)
16010     NEXT DL
16020     IF flags$="D" THEN ENDPROC  : REM No need to reset DL$ as it was never checked after
16025     REM *WIPE*   (original code clears three lines from line 21 to line 23)
16030     PRINT TAB(0,22) SPC(MAPWIDTH%)
16040     PRINT TAB(0,23) SPC(MAPWIDTH%)
16050     PRINT TAB(0,24) SPC(MAPWIDTH%)
16060     IF flags$="W" THEN ENDPROC  : REM No need to reset DL$ as it was never checked after
16070     IF CS>77-INT(2*P1^2.5) THEN CS=77-INT(2*P1^2.5)
16080     IF PS<7 THEN PS=7
16090     IF PS>INT(42*(P1+1)^LOG(P1^3.7))+75 THEN PS=INT(42*(P1+1)^LOG(P1^3.7))+75
16100     IF STAMINA>125-(INT(P1)*12.5) THEN STAMINA=125-INT(INT(P1)*12.5)
16105 COLOUR 15   : REM Bright white
16110     PRINT TAB(0,15) NAME$;" (";CLASS$;")"
16115 COLOUR 11   : REM Bright yellow
16120     PRINT "TREASURE    ";TS,"COMBAT STR  ";CS;" "
16130     PRINT "EXPERIENCE  ";EX,"PSI POWER   ";PS;" "
16140     PRINT "TURNS       ";TURNS,"STAMINA     ";STAMINA;" "
16150     PRINT "SPELLS      [S]";
16152     IF EX >= 1000 AND PS >= 49 THEN PRINT "[P]";
16154     IF EX >= 5000 AND PS >= 77 THEN PRINT "[C]";
16179     REM If fighting, then update monster
16180     IF FIGHTING=1 THEN GOTO 16210
16190     PRINT TAB(0, 20) SPC(MAPWIDTH%) 
16200     ENDPROC
16210     REM PRINT USING"STRENGTH ## ##";MS,MM
16215 COLOUR 15   : REM brigth white
16220     PRINT TAB(0, 20) M$;" - STRENGTH = ";MS;" ";
16225     IF MM > 0 THEN PRINT ", MAGIC = ";MM;"   "
16230 ENDPROC
16998 :
16999 REM VPOKE replacement
17000 DEF PROC_vpoke(addr%, val%)
17005     LOCAL X1%, Y1%
17006     IF val% < 32 THEN ENDPROC
17010     X1% = addr% MOD MAPWIDTH%
17020     Y1% = addr% / MAPWIDTH%
17030     REM PRINT TAB(X1%, Y1%) : REM Could use VDU 31?
17040     PRINT TAB(X1%, Y1%) CHR$(val%)
17045     MAP%(addr%) = val%                  : REM Update shadow buffer
17050 ENDPROC
17060 DEF PROC_vpokeXY(X%, Y%, val%)
17065     IF val% < 32 THEN ENDPROC
17070     PRINT TAB(X%, Y%) CHR$(val%)
17075     MAP%(Y% * MAPWIDTH% + X%) = val%    : REM Update shadow buffer
17080 ENDPROC
17098 :
17099 REM VPEEK replacement
17100 DEF FN_vpeek(addr%)
17105     REM LOCAL X%, Y%, V$, V%
17106     REM X% = addr% MOD MAPWIDTH%
17107     REM Y% = addr% / MAPWIDTH%
17111     REM VDU 23, 0, &83, x; y;
17114     REM V$ = GET$(1, 16)
17115     REM V% = VAL(GET$(1, 16))
17116     V% = MAP%(addr%)
17110     REM PRINT TAB(20, 1);addr%;" ";V%;"  "
17120 =V%
17198 :
17199 REM REplacement for TAB(0,21) PRINT "zzz..."
17200 DEF PROC_statusPrint(msg$)
17205    COLOUR 15  : REM White
17210    PRINT TAB(0,22) msg$;  : REM semicolon necessary for when inputting key (cursor)
17220 ENDPROC
17299 :
17300 DEF PROC_statusClear
17310    PRINT TAB(0,22) SPC(MAPWIDTH%)
17320 ENDPROC
17399 :
17400 DEF PROC_disableCursor
17410    VDU 23, 1, 0
17420 ENDPROC
17499 :
17500 DEF PROC_enableCursor
17510    VDU 23, 1, 1
17520 ENDPROC
17998 :
17999 REM Load custom character gfx
18000 DEF PROC_defineCustomChars
18003     LOCAL A%, N0%, N1%, N2%, N3%, N4%, N5%, N6%, N7%
18005     RESTORE 31000
18010     FOR I% = 1 TO 22
18020         READ A%, N0%, N1%, N2%, N3%, N4%, N5%, N6%, N7%
18025         PRINT "Redefining ";A%
18030         VDU 23, A%, N0%, N1%, N2%, N3%, N4%, N5%, N6%, N7%
18040     NEXT I%
18050 ENDPROC
18998 :
18999 REM Clear the shadow character buffer
19000 DEF PROC_clearMap
19005     LOCAL A% : A% = MAPWIDTH% * MAPHEIGHT% - 1
19010     FOR I% = 0 TO A% : MAP%(I%) = 32 : NEXT I%
19020 ENDPROC
19998 :
19999 REM *** Quit *(safe castle) (was 48000)
20000 PROC_statusPrint("THOU ART SAFE IN A CASTLE") : IF CS<20 THEN CS=20
20010 PROC_vpoke(M, PK) : PK=FN_vpeek(W) : M=W : PROC_vpoke(M, Q)
20020 PRINT TAB(0, 23) "WILT THOU LEAVE THE VALLEY? (Y/N) ";
20030 GC$ = FN_getFilteredKey("YyNn") : REM ** uniget
20040 PROC_delayUpdate(5, "W")
20049 REM * Generate rating in case of save
20050 PROC_showRating
20060 PROC_delayUpdate(110, "W")
20070 IF GC$ = "Y" OR GC$ = "y" THEN 22000 : REM ** Save + exit
20080 STAMINA=150 : PROC_statusPrint("THY WOUNDS HEALED... THY SWORD SHARP")
20090 PRINT "GO AS THE GODS DEMAND..."
20100 PROC_delayUpdate(120, "")
20110 GOTO 2010 : REM ** Movement
20998 :
20999 REM *** Rating routine   (was 45000)
21000 DEF PROC_showRating
21010     PROC_delayUpdate(5, "W")
21020     RATING=INT(.067*(EX+TS/3)^.5+LOG(EX/((TURNS+1)^1.5)))
21030     IF RATING>28 THEN RATING=28
21040     IF RATING<0 THEN RATING=0
21045     COLOUR 10
21050     PRINT TAB(0,22) "YOUR RATING NOW BE ";RATING;"/28"
21060     IF T(2)=1 THEN PRINT "YOU HAVE THE HELM OF EVANNA!"
21070     IF T(0)=1 THEN PRINT "YOU HAVE THE AMULET WITH ";T(1);"/6 STONES."
21080     PROC_delayUpdate(250, "W")
21089     REM JH - Not sure how this works so I've cvommented it out
21090     REM IF GC$="E" THEN STAMINA=STAMINA-10 : GC$="" : GOTO 2010
21100 ENDPROC
21997 :
21998 REM These functions were 50000 and 55000 
21999 REM ******* Save character routine goes here ***  (was 50000)
22000 CLS : COLOUR 15 : REM White
22010 PRINT "DO YOU WISH TO SAVE, ";NAME$;"? (Y/N) ";
22020 GC$ = FN_getFilteredKey("YyNn") : REM ** uniget
22030 IF GC$ = "N" OR GC$ = "n" THEN 22100
22050 FH = OPENOUT "VALLEY.SAV" : IF FH = 0 THEN ENDPROC
22060     PRINT#FH, NAME$, CLASS$, TS, EX, TURNS, CS, PS, T(0), T(1), T(2), P1, C1
22070 CLOSE#FH
22080 PRINT : PRINT "*** DONE ***"
22100 PRINT : PRINT "TYPE RUN TO START AGAIN."
22110 END
22998 :
22999 REM *** Death routine  (was 55000)
23000 STAMINA=0 : CS=0 : PS=0 : FIGHTING=0 : PROC_delayUpdate(110, "") : IF T(1)=6 THEN 23070
23030 PRINT TAB(1,22) "OH WHAT A FRAIL SHELL" : PRINT TAB(12);"IS THIS WHAT WE CALL MAN"
23040 PROC_delayUpdate(300, "W")
23050 CLS : END
23069 REM *** Restore character to life
23070 T(0)=0 : T(1)=0 : TS=0 : CS=30 : STAMINA=150 : PS=30
23080 PRINT TAB(0,22) "ALARIAN'S AMULET PROTECTS THY SOUL":PRINTCHR$(27)+"p";"  LIVE AGAIN  ";CHR$(27)+"q"
23090 PROC_delayUpdate(150, "")
23100 L=G(0) : MP=L : M=W : S=1
23110 GOTO 9220 : REM scene control
29997 :
29998 REM DATA MOVED FROM 60000 TO HERE, AS RESTORE cannot handle anything over 32767
29999 REM "Scenario data" (for castle type scenes)
30000 DATA 4,7,3,6,4,4,6,5,3,6,0,3,8,4,3,5,5,3,8,3,4,5,0,6,3,6,4,6,4,7,4,100
30009 REM "Monster data"
30010 DATA AWOLFEN,9,0,AHOBGOBLIN,9,0,AORC,9,0,EFIRE-IMP,7,3,GROCK-TROLL,19,0
30020 DATA EHARPY,10,12,AOGRE,23,0,BBARROW-WIGHT,0,25,HCENTAUR,18,14
30030 DATA EFIR-GIANT,20,20,VTHUNDER-LIZARD,50,0,CMINOTAUR,35,25,CWRAITH,0,30
30040 DATA FWYVERN,36,12,BDRAGON,50,20,CRING-WRAITH,0,45,ABALROG,50,50
30050 DATA LWATER-IMP,15,15,LKRAKEN,50,0
30997:
30998 REM Data for custom characters
30999 REM Border (char 128)
31000 DATA 128, &20,&31,&7B,&FF,&F7,&EF,&FF,&FF
31009 REM Player (char 129 to 134, for the 5 character classes, + swimming player)
31010 DATA 129, &3A,&3A,&12,&FF,&BA,&38,&28,&6C
31011 DATA 130,255,255,255,255,255,255,255,255
31012 DATA 131,255,255,255,255,255,255,255,255
31013 DATA 132,255,255,255,255,255,255,255,255
31014 DATA 133,255,255,255,255,255,255,255,255
31015 DATA 134, &38,&38,&BA,&55,&BA,&00,&52,&A4
31019 REM Path diagonals (char 135 and 136)
31020 DATA 135, &80,&40,&20,&10,&08,&04,&02,&01
31021 DATA 136, &01,&02,&04,&08,&10,&20,&40,&80 
31029 REM Woods (char 137)
31030 DATA 137, &7E,&BB,&EF,&FF,&5A,&18,&18,&3C
31039 REM Swamp (char 138)
31040 DATA 138, &A0,&45,&02,&28,&92,&01,&50,&20
31049 REM Lair (the lair in the woods and the swamp) (char 139)
31050 DATA 139, &18,&3C,&7E,&7E,&FF,&FF,&E7,&E7
31059 REM The Black Tower (char 140) (was 87 in original)
31060 DATA 140, &A5,&FF,&FF,&DB,&FF,&FF,&FF,&E7
31069 REM Doorway in tower (char 141) (was 104 in original)
31070 DATA 141, &FF,&E7,&C3,&C3,&C3,&D3,&C3,&C3
31079 REM Woods / swamp border (char 142) (was 96 in original)
31080 DATA 142, &2A,&55,&AA,&55,&AA,&55,&AA,&54
31089 REM Safe Castle (char 143) (was 219 in orig)
31090 DATA 143, &A5,&E7,&E7,&FF,&BD,&FF,&A5,&E7
31099 REM Tree (char 144) (was 88 in orig)
31100 DATA 144, &10,&38,&10,&7C,&10,&FE,&10,&14
31109 REM Water (char 145) (was 224 in orig)
31110 REM DATA 145, &14,&28,&00,&25,&4A,&00,&52,&A4
31111 DATA 145, &60,&90,&09,&06,&30,&48,&84,&03
31119 REM Tufts in swamp(char 146) (was 45 in orig)
31120 DATA 146, &20,&13,&14,&78,&9C,&1A,&18,&3C
31129 REM Tower border/walls (char 147) (was 160 in orig)
31130 DATA 147, &00,&BB,&BB,&BB,&00,&EE,&EE,&EE
31139 REM Tower stairs (char 148) (was 102 in orig)
31140 DATA 148, &7E,&42,&7E,&42,&7E,&42,&7E,&00
31149 REM Treasure (char 149) (was 42 in orig)
31150 DATA 149, &14,&40,&19,&BC,&3C,&7E,&7E,&00




