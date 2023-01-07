      ******************************************************************
      * Author:
      * Date:
      * Purpose: split hourly power usage into virtual Tempo billing, and compare
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. tempo.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TicCsv ASSIGN TO CsvFilename
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  TicCsv RECORD IS VARYING IN SIZE.
       01  TicRec                  PIC X(100).
           88 EndOfFile            VALUE HIGH-VALUES.

       WORKING-STORAGE SECTION.
       01  CsvFilename             PIC X(32).

       01  TicData.
      *     2022-01-01T07:00:00+01:00 - Conso from HH:00:00 to HH:59:59
      * example: 2022-01-01T05:00:00+01:00,752
           02 RFC3339DT.
               03 RFCDate.
                   04 DateYear     PIC 9999.
                   04 FILLER       PIC X VALUE "-".
                   04 DateMonth    PIC 99.
                   04 FILLER       PIC X VALUE "-".
                   04 DateDay      PIC 99.
               03 FILLER           PIC X VALUE "T".
               03 RFCTime.
                   04 TimeHour     PIC 99.
                       88 IsHP     VALUE 6 THRU 21.
                   04 FILLER       PIC X VALUE ":".
                   04 TimeMinute   PIC 99.
                   04 FILLER       PIC X VALUE ":".
                   04 TimeSecond   PIC 99.
               03 FILLER           PIC X VALUE "+".
               03 FILLER           PIC X(5).
           02 ConsWh               PIC 9(6).

       01  ConsoConstants.
           02 ConsoNameValues.
               03 FILLER           PIC X(10)       VALUE "Base".
               03 FILLER           PIC X(10)       VALUE "Rouge HP".
               03 FILLER           PIC X(10)       VALUE "Rouge HC".
               03 FILLER           PIC X(10)       VALUE "Blanc HP".
               03 FILLER           PIC X(10)       VALUE "Blanc HC".
               03 FILLER           PIC X(10)       VALUE "Bleu HP".
               03 FILLER           PIC X(10)       VALUE "Bleu HC".
           02 FILLER REDEFINES ConsoNameValues.
               03 ConsoName        PIC X(10) OCCURS 7 TIMES.
           02 YearlyCostsValues USAGE IS COMPUTATIONAL.
               03 Costs2022.
                   04 TarifAboBase         PIC 999V99      VALUE 169.92.
                   04 TarifAboTempo        PIC 999V99      VALUE 182.88.
                   04 CostKwhValues.
                       05 CostKwhBase      PIC 9V9999      VALUE 0.1740.
                       05 CostKwhRHP       PIC 9V9999      VALUE 0.6274.
                       05 CostKwhRHC       PIC 9V9999      VALUE 0.1509.
                       05 CostKwhWHP       PIC 9V9999      VALUE 0.1773.
                       05 CostKwhWHC       PIC 9V9999      VALUE 0.1412.
                       05 CostKwhBHP       PIC 9V9999      VALUE 0.1498.
                       05 CostKwhBHC       PIC 9V9999      VALUE 0.1231.
               03 Costs2023.
                   04 TarifAboBase         PIC 999V99      VALUE 169.89.
                   04 TarifAboTempo        PIC 999V99      VALUE 177.84.
                   04 CostKwhValues.
                       05 CostKwhBase      PIC 9V9999      VALUE 0.1740.
                       05 CostKwhRHP       PIC 9V9999      VALUE 0.5486.
                       05 CostKwhRHC       PIC 9V9999      VALUE 0.1222.
                       05 CostKwhWHP       PIC 9V9999      VALUE 0.1653.
                       05 CostKwhWHC       PIC 9V9999      VALUE 0.1112.
                       05 CostKwhBHP       PIC 9V9999      VALUE 0.1272.
                       05 CostKwhBHC       PIC 9V9999      VALUE 0.0862.
           02 FILLER REDEFINES YearlyCostsValues USAGE IS COMPUTATIONAL.
               03 YearlyCosts OCCURS 2 TIMES.
                   04 CostAboBase          PIC 999V99.
                   04 CostAboTempo         PIC 999V99.
                   04 CostKwh              PIC 9V9999 OCCURS 7 TIMES.

       01  ConsoData USAGE IS COMPUTATIONAL.
           02 ConsoSums OCCURS 7 TIMES.
               03 CTotalWh         PIC 9(9)        VALUE ZEROES.
               03 CTotalKWh        REDEFINES CTotalWh PIC 9(6)V999.
               03 CTotalHours      PIC 9(4)        VALUE ZEROES.

       01  YearIdx                 USAGE IS INDEX.
       01  TableIdx                USAGE IS INDEX.

       01  CouleurJour             PIC X.
           88 JourRouge            VALUE "R".
           88 JourBlanc            VALUE "W".
           88 JourBleu             VALUE "B".

       01  TempoSums USAGE IS COMPUTATIONAL.
           02 TempoTotalWh         PIC 9(10).
           02 TempoTotalKWh        REDEFINES TempoTotalWh PIC 9(7)V999.
           02 TempoTotalCost       PIC 9(6)V9999.

       01  TotalCost   PIC 9(6)V9999   COMP OCCURS 7 TIMES.
       01  TempCost    PIC S9(6)V9999  COMP.

       01  ReportHeading USAGE IS DISPLAY.
           02 FILLER               PIC X(10)       VALUE "Sum".
           02 FILLER               PIC X(10)       VALUE "       KWh".
           02 FILLER               PIC X(11)       VALUE "      Cost ".
           02 FILLER               PIC X(8)        VALUE "   Hours".
           02 FILLER               PIC X(8)        VALUE "     $/h".
           02 FILLER               PIC X(4)        VALUE "   T".
           02 PrnYear              PIC 9999.

       01  DetailLine USAGE IS DISPLAY.
           02 PrnName              PIC X(10).
           02 PrnTotalKwh          PIC B(3)ZZZBZZ9.
           02 PrnTotalCost         PIC $$$B$$9.99-.
           02 PrnTotalHours        PIC B(3)ZBZZ9.
           02 PrnCostPerHour       PIC B(2)$$9.99.


       PROCEDURE DIVISION.
       Begin.
           DISPLAY "File name? " WITH NO ADVANCING
           ACCEPT CsvFilename

           OPEN INPUT TicCsv
           READ TicCsv
      *    ignore first line  (header)
           READ TicCsv AT END
               DISPLAY "File is empty!"
               CLOSE TicCsv
               STOP RUN
           END-READ
           PERFORM UNTIL EndOfFile
               MOVE SPACES TO TicData
               UNSTRING TicRec DELIMITED BY "," INTO RFC3339DT, ConsWh
               PERFORM SetCouleurJour
               PERFORM CalculateConsoSums
               READ TicCsv AT END SET EndOfFile TO TRUE END-READ
           END-PERFORM
           CLOSE TicCsv

           PERFORM VARYING YearIdx FROM 1 BY 1 UNTIL YearIdx > 2
               ADD 2021 TO YearIdx GIVING PrnYear
               DISPLAY ReportHeading
               INITIALIZE TempoSums
               PERFORM YearIdxTempoDetails
               DISPLAY SPACE
               PERFORM YearIdxGrandTotals
               DISPLAY SPACE
               PERFORM YearIdxSavings
               DISPLAY SPACE
           END-PERFORM

           STOP RUN
           .

       YearIdxGrandTotals.
      *    Process "Base"
           COMPUTE TotalCost(1) = CTotalKWh(1) * CostKwh(YearIdx, 1)
           MOVE 1 TO TableIdx
           PERFORM TableIdxDisplayLine

      *    This "works" because USAGE IS DISPLAY means numbers are ASCII so spaces are spaces when displayed
           MOVE SPACES TO DetailLine
           MOVE "Tempo" TO PrnName
           MOVE TempoTotalKWh TO PrnTotalKwh
           ADD ZERO TO TempoTotalCost GIVING PrnTotalCost ROUNDED
           DISPLAY DetailLine
           .

       YearIdxSavings.
           MOVE SPACES TO DetailLine
           MOVE "Savings" TO PrnName
           SUBTRACT TotalCost(1) FROM TempoTotalCost
               GIVING PrnTotalCost ROUNDED
           DISPLAY DetailLine

           MOVE "Diff abo" TO PrnName
           SUBTRACT CostAboBase(YearIdx) FROM CostAboTempo(YearIdx)
               GIVING TempCost
           ADD ZERO To TempCost GIVING PrnTotalCost ROUNDED
           DISPLAY DetailLine

           MOVE "Net" TO PrnName
           ADD TempCost TO TempoTotalCost
           SUBTRACT TotalCost(1) FROM TempoTotalCost
               GIVING PrnTotalCost ROUNDED
           DISPLAY DetailLine
           .

       YearIdxTempoDetails.
           PERFORM VARYING TableIdx FROM 2 BY 1 UNTIL TableIdx > 7
               COMPUTE TotalCost(TableIdx) = CTotalKWh(TableIdx)
                   * CostKwh(YearIdx, TableIdx)

               ADD CTotalWh(TableIdx) TO TempoTotalWh
               ADD TotalCost(TableIdx) TO TempoTotalCost

               PERFORM TableIdxDisplayLine
           END-PERFORM
           .

       TableIdxDisplayLine.
           MOVE CTotalKWh(TableIdx) TO PrnTotalKwh
           ADD ZERO TO TotalCost(TableIdx) GIVING PrnTotalCost ROUNDED
           DIVIDE TotalCost(TableIdx) BY CTotalHours(TableIdx)
               GIVING PrnCostPerHour ROUNDED
           MOVE ConsoName(TableIdx) TO PrnName
           MOVE CTotalHours(TableIdx) TO PrnTotalHours
           DISPLAY DetailLine
           .

       CalculateConsoSums.
           EVALUATE TRUE ALSO TRUE
               WHEN JourRouge ALSO IsHP
                   MOVE 2 TO TableIdx
               WHEN JourRouge ALSO NOT IsHP
                   MOVE 3 TO TableIdx
               WHEN JourBlanc ALSO IsHP
                   MOVE 4 TO TableIdx
               WHEN JourBlanc ALSO NOT IsHP
                   MOVE 5 TO TableIdx
               WHEN JourBleu ALSO IsHP
                   MOVE 6 TO TableIdx
               WHEN JourBleu ALSO NOT IsHP
                   MOVE 7 TO TableIdx
           END-EVALUATE
           ADD ConsWh TO CTotalWh(1)
           ADD ConsWh TO CTotalWh(TableIdx)
           ADD 1 TO CTotalHours(1)
           ADD 1 TO CTotalHours(TableIdx)
           .

       SetCouleurJour.
           EVALUATE DateMonth ALSO TRUE
               WHEN 1
                   ALSO DateDay = 6 OR 10 OR 11 OR 12 OR 13 OR 14 OR 17
                   OR 18 OR 19 OR 20 OR 21 OR 24 OR 25 OR 26 OR 27
                       SET JourRouge TO TRUE
               WHEN 1
                   ALSO DateDay = 5 OR 7 OR 15 OR 22 OR 28 OR 31
                       SET JourBlanc TO TRUE
               WHEN 2
                   ALSO DateDay = 2 OR 3 OR 7 OR 8 OR 9 OR 10 OR 11
                   OR 23 OR 25 OR 28
                       SET JourBlanc TO TRUE
               WHEN 3
                   ALSO DateDay = 1 OR 2 OR 3 OR 7 OR 8
                       SET JourBlanc TO TRUE
               WHEN 4
                   ALSO DateDay = 4 OR 5 OR 6 OR 14
                       SET JourBlanc TO TRUE
               WHEN 5
                   ALSO DateDay = 24 OR 30 OR 31
                       SET JourBlanc TO TRUE
               WHEN 11
                   ALSO DateDay = 29 OR 30
                       SET JourBlanc TO TRUE
               WHEN 12
                   ALSO DateDay = 8 OR 12 OR 13 OR 14
                       SET JourRouge TO TRUE
               WHEN 12
                   ALSO DateDay = 1 OR 2 OR 5 OR 6 OR 7 OR 9 OR 10
                   OR 15 OR 16 OR 17
                       SET JourBlanc TO TRUE
               WHEN OTHER
                   SET JourBleu TO TRUE
           END-EVALUATE
           .

       END PROGRAM tempo.

      * HP: 6-22h, HC: 22-6h
      * 22j rouges, 43j blancs, 300j bleus
      *
      * Jours Blancs
      * Dec 2022: 1, 2, 5, 6, 7, 9, 10, 15, 16, 17
      * Nov 2022: 29, 30
      * Mai 2022: 24, 30, 31
      * Avr 2022: 4, 5, 6, 14
      * Mar 2022: 1, 2, 3, 7, 8
      * Fev 2022: 2, 3, 7, 8, 9, 10, 11, 23, 25, 28
      * Jan 2022: 5, 7, 15, 22, 28, 31
      *
      * Jours Rouges
      * Dec 2022: 8, 12, 13, 14
      * Jan 2022: 6, 10, 11, 12, 13, 14, 17, 18, 19, 20, 21, 24, 25, 26, 27
