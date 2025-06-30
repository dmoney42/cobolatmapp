       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATMAPP.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE ASSIGN TO "accounts.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT TRANSACTION-LOG ASSIGN TO "transactions.log"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS TRANS-STAT.  

           SELECT RECEIPT-FILE ASSIGN TO "atm-receipt.txt"
               ORGANIZATION IS LINE SEQUENTIAL.



       DATA DIVISION.
       FILE SECTION.
       FD ACCOUNT-FILE
           RECORD CONTAINS 49 CHARACTERS
           DATA RECORD IS ACCOUNT-RECORD.
       01 ACCOUNT-RECORD.
           05 USER-ID             PIC X(16). *> 16 characters
           05 PIN                 PIC X(4). *> 4 characters
           05 CHECKING-BALANCE      PIC 9(5)V99.
           05 SAVINGS-BALANCE       PIC 9(5)V99.
           05 DAILY-WITHDRAW-AMOUNT PIC 9(5)V99.
           05 LAST-WITHDRAW-DATE  PIC 9(8).

       FD TRANSACTION-LOG
           RECORD CONTAINS 38 CHARACTERS
           DATA RECORD IS TRANSACTION-RECORD.
       01 TRANSACTION-RECORD.
           05 TR-TIMESTAMP      PIC X(16).
           05 TR-CARD-LAST4     PIC X(4).
           05 TR-TYPE           PIC X.
           05 TR-AMOUNT         PIC 9(5)V99.
           05 TR-NEW-BALANCE    PIC 9(5)V99.
           05 TR-END-MARKER     PIC X(3).
       
       *> This defines the template for the receipt text document
       FD RECEIPT-FILE.
         01 RECEIPT-LINE         PIC X(80).
      *>********************************************* 

       WORKING-STORAGE SECTION.
       01 MENU-CHOICE        PIC X.
       01 DEPOSIT-AMOUNT     PIC 9(5)V99.
       01 TEMP-BALANCE       PIC 9(5)V99.
       01 WITHDRAW-CHOICE    PIC X.
       01 WITHDRAW-AMOUNT    PIC 9(5)V99.
       01 REMAINDER20        PIC 9(3).
       01 REMAINDER50        PIC 9(3).
       01 WS-TODAY           PIC 9(8).
       01 DAILY-LIMIT        PIC 9(5)v99 VALUE 500.00.
       01 REMAINING-LIMIT     PIC 9(5)V99.
       01 ENTERED-CARD-NUMBER   PIC X(16).
       01 ENTERED-PIN           PIC X(4).
       01 PIN-ATTEMPTS          PIC 9 VALUE 0.
       01 MAX-ATTEMPTS          PIC 9 VALUE 3.
       01 MATCH-FOUND           PIC X VALUE "N".
            88 RECORD-MATCH       VALUE "Y".
            88 NO-MATCH           VALUE "N". 
       01 WS-SAVED-RECORD     PIC X(42). 
       *> Must match ACCOUNT-RECORD size

       *> Used to build the transaction log line
       01 WS-TR-TIMESTAMP     PIC X(16).
       01 WS-TR-CARD-LAST4    PIC X(4).
       01 WS-TR-TYPE          PIC X.
       01 WS-TR-AMOUNT        PIC 9(5)V99.
       01 WS-TR-NEW-BALANCE   PIC 9(5)V99.
       
       01 WS-TRANSACTION-DATA.
           05 FILLER          PIC X(16).  *> Align with WS-TR-TIMESTAMP
           05 FILLER          PIC X(4).
           05 FILLER          PIC X.
           05 FILLER          PIC 9(5)V99.
           05 FILLER          PIC 9(5)V99.

       *> these are the temporary storage variables 
       01 WS-RECEIPT-LINE     PIC X(80).
       01 WS-FORMATTED-DATE   PIC X(10).  *> YYYY/MM/DD
       01 WS-FORMATTED-TIME   PIC X(8).   *> HH:MM:SS
       01 WS-DISPLAY-AMOUNT     PIC Z(5).99.
       01 WS-DISPLAY-BALANCE    PIC Z(5).99.


       *> this variable is for debugging to see what file status code 
       *> we will get when we try to write to a file
       01 TRANS-STAT PIC XX.

       *> This variable for accepting an input to allow the user to 
       *> 1 for checking and 2 for savings
       01 ACCOUNT-TYPE         PIC X.

       *>This variable is for our ATM receipt that will display
       *> which account we withdrew from
       01 WS-TR-ACCOUNT-TYPE    PIC X(8).  *> display CHECKING or SAVINGS
       
       *>This variable is for the ATM receipt to store and display
       *> WITHDRAWAL or DEPOSIT
       01 WS-TYPE-DESCRIPTION     PIC X(10).

       *> These variables are for transferring funds.
       01 TRANSFER-CHOICE      PIC X. *> trasfer to checkings or savings
       01 TRANSFER-AMOUNT      PIC 9(5)V99. *> amount to transfer

       01 WS-DISPLAY-AMOUNT-TXT  PIC X(10).
       01 WS-DISPLAY-BALANCE-TXT PIC X(10).


       01 USER-INPUT-STR       PIC X(10).   *> Raw input from user (string)
       01 NUMERIC-FLAG         PIC X.      *> 'Y' if valid number, 'N' otherwise
           88 IS-NUMERIC        VALUE 'Y'.
           88 NOT-NUMERIC       VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "ATM APP STARTED".
           OPEN INPUT ACCOUNT-FILE.

      *    Enter card number then we search the database
           DISPLAY "Welcome to the ATM"
           DISPLAY "Enter your 16-digit card number"
           ACCEPT ENTERED-CARD-NUMBER

           PERFORM WITH TEST AFTER UNTIL RECORD-MATCH OR NO-MATCH
              READ ACCOUNT-FILE
                    AT END
                        SET NO-MATCH TO TRUE
                        DISPLAY "End of file reached."
                    NOT AT END
                    DISPLAY "I read the doc. The Username is: " USER-ID

                     IF ENTERED-CARD-NUMBER = USER-ID
                         SET RECORD-MATCH TO TRUE
                         MOVE ACCOUNT-RECORD TO WS-SAVED-RECORD
                         DISPLAY "Card number recognized!"

                       *> --- NEW: PIN Verification ---
                       PERFORM UNTIL ENTERED-PIN = PIN OR PIN-ATTEMPTS 
                       >= MAX-ATTEMPTS
                          DISPLAY "Enter your 4-digit PIN: "
                          ACCEPT ENTERED-PIN
                          IF ENTERED-PIN NOT = PIN
                             ADD 1 TO PIN-ATTEMPTS
                             DISPLAY "You attempted your pin: " 
                             PIN-ATTEMPTS
                             DISPLAY " times."
                            DISPLAY "Incorrect PIN. Attempts left: " 
                            FUNCTION NUMVAL(MAX-ATTEMPTS - PIN-ATTEMPTS)
                          END-IF
                       END-PERFORM    

                       *> Exit if PIN attempts exhausted
                       IF PIN-ATTEMPTS >= MAX-ATTEMPTS
                          DISPLAY "Too many failed attempts. Exiting."
                          CLOSE ACCOUNT-FILE
                          STOP RUN
                       END-IF

                     END-IF *> end of ENTERED-CARD-NUMBER = USER-ID
               END-READ
           END-PERFORM.
           
           IF NO-MATCH
               DISPLAY "Card number not recognized. Exiting."
               CLOSE ACCOUNT-FILE
               STOP RUN
           END-IF  

           CLOSE ACCOUNT-FILE.         
     

           ACCEPT WS-TODAY FROM DATE.

           IF LAST-WITHDRAW-DATE NOT = WS-TODAY
               MOVE 0 TO DAILY-WITHDRAW-AMOUNT
               MOVE WS-TODAY TO LAST-WITHDRAW-DATE
           END-IF.

           *> after the user validates their card and pin, then we ask
           *> the user what account they want to access. If they dont
           *> enter 1 or 2 then it defaults to 1 (checking)
           DISPLAY "Choose an account:"
           DISPLAY "1. Checking"
           DISPLAY "2. Savings"
           ACCEPT ACCOUNT-TYPE
           
           IF ACCOUNT-TYPE NOT = "1" AND ACCOUNT-TYPE NOT = "2"
             DISPLAY "Invalid selection. Defaulting to Checking."
             MOVE "1" TO ACCOUNT-TYPE
           END-IF.
           

           PERFORM UNTIL MENU-CHOICE = "5"
              DISPLAY " "
              DISPLAY "===== ATM MENU ====="
              DISPLAY "1. Check Balance"
              DISPLAY "2. Deposit"
              DISPLAY "3. Withdraw"
              DISPLAY "4. Transfer Funds Between Accounts"
              DISPLAY "5. Exit"
              DISPLAY "Choose an option (1-5): "
              ACCEPT MENU-CHOICE

                  EVALUATE MENU-CHOICE
                     WHEN "1"

                       IF ACCOUNT-TYPE = "1"
                         DISPLAY "Your balance is: $" CHECKING-BALANCE
                       ELSE
                          DISPLAY "Your balance is: $" SAVINGS-BALANCE
                       END-IF

                     WHEN "2"
                        DISPLAY "Enter amount to deposit: "

                        *>ACCEPT DEPOSIT-AMOUNT
                         *> ***************************************  
                        *> section for input validation for deposit
                        ACCEPT USER-INPUT-STR

                         IF FUNCTION NUMVAL(USER-INPUT-STR) > 0
                            MOVE 'Y' TO NUMERIC-FLAG
                            MOVE FUNCTION NUMVAL(USER-INPUT-STR) TO 
                                                          DEPOSIT-AMOUNT
                         ELSE 
                            MOVE 'N' TO NUMERIC-FLAG
                         END-IF

                         IF NOT-NUMERIC
                           DISPLAY "Invalid input."
                           DISPLAY "Please enter a number."
                         ELSE                  
                         *> ***************************************

                         IF ACCOUNT-TYPE = "1"
                             COMPUTE CHECKING-BALANCE = CHECKING-BALANCE
                              + DEPOSIT-AMOUNT
                             DISPLAY "New balance: $" CHECKING-BALANCE
                         ELSE
                             COMPUTE SAVINGS-BALANCE = SAVINGS-BALANCE 
                             + DEPOSIT-AMOUNT
                             DISPLAY "New balance: $" SAVINGS-BALANCE
                         END-IF

                         MOVE FUNCTION CURRENT-DATE(1:16) TO 
                                                        WS-TR-TIMESTAMP
                         MOVE 'D' TO WS-TR-TYPE
                         MOVE DEPOSIT-AMOUNT TO WS-TR-AMOUNT
                         MOVE USER-ID(13:4) TO WS-TR-CARD-LAST4
                         
                         IF ACCOUNT-TYPE = "1"
                           MOVE CHECKING-BALANCE TO WS-TR-NEW-BALANCE
                           MOVE "CHECKING" TO WS-TR-ACCOUNT-TYPE
                         ELSE
                           MOVE SAVINGS-BALANCE TO WS-TR-NEW-BALANCE
                           MOVE "SAVINGS" TO WS-TR-ACCOUNT-TYPE
                         END-IF
                         
                         
                         IF WS-TR-TYPE = "W"
                           MOVE "WITHDRAWAL" TO WS-TYPE-DESCRIPTION
                         ELSE
                           MOVE "DEPOSIT" TO WS-TYPE-DESCRIPTION
                         END-IF
                         
                         PERFORM LOG-TRANSACTION
                         PERFORM WRITE-RECEIPT
                         PERFORM SAVE-BALANCE

                        
                     WHEN "3"
                       MOVE " " TO WITHDRAW-CHOICE
                       PERFORM UNTIL WITHDRAW-CHOICE = "6" 
                          DISPLAY " "
                          DISPLAY "Select Widthdrawal Amount"
                          DISPLAY "Choose an option (1-6)"
                          DISPLAY "1. $20"
                          DISPLAY "2. $50"
                          DISPLAY "3. $80"
                          DISPLAY "4. $100"
                          DISPLAY "5. Custom Amount"
                          DISPLAY "6. Back to Menu"

                          ACCEPT WITHDRAW-CHOICE


                          EVALUATE WITHDRAW-CHOICE
                             WHEN "1"
                               MOVE 20.00 TO WITHDRAW-AMOUNT
                               PERFORM PROCESS-WITHDRAWAL
                             WHEN "2"
                               MOVE 50.00 TO WITHDRAW-AMOUNT
                               PERFORM PROCESS-WITHDRAWAL  
                             WHEN "3"
                               MOVE 80.00 TO WITHDRAW-AMOUNT
                               PERFORM PROCESS-WITHDRAWAL    
                             WHEN "4"
                               MOVE 100.00 TO WITHDRAW-AMOUNT
                               PERFORM PROCESS-WITHDRAWAL     
                             WHEN "5"
                               PERFORM CUSTOM-WITHDRAW                 
                             WHEN "6"
                                DISPLAY "Returning to main menu..."
                             WHEN OTHER
                                DISPLAY "Invalid option. Try again" 
                          END-EVALUATE
                       END-PERFORM

                     WHEN "4"
                     PERFORM TRANSFER-FUNDS

                     WHEN "5"
                       DISPLAY "Exiting... Goodbye."

                     WHEN OTHER
                       DISPLAY "Invalid option. Please try again."
                  END-EVALUATE
           END-PERFORM.           
           
           STOP RUN.


      ******PARAGRAPHS*****************************************
      *********************************************************
      *********************************************************      
      *These are our PARAGRAPHS that can be called from anywhere
      * similar to functions
       PROCESS-WITHDRAWAL.
           
           COMPUTE REMAINING-LIMIT = DAILY-LIMIT - WITHDRAW-AMOUNT

           IF WITHDRAW-AMOUNT > REMAINING-LIMIT
              DISPLAY "You've reached your daily withdrawal limit"
              MOVE "6" TO WITHDRAW-CHOICE
           ELSE IF REMAINING-LIMIT = 0
                 DISPLAY "Amount exceeds daily limit. You can withdraw "
                 DISPLAY "up to: $" REMAINING-LIMIT
                 MOVE "6" TO WITHDRAW-CHOICE
           ELSE IF ACCOUNT-TYPE = "1"
                       IF CHECKING-BALANCE >= WITHDRAW-AMOUNT
                           COMPUTE CHECKING-BALANCE = CHECKING-BALANCE 
                                                     - WITHDRAW-AMOUNT
                           DISPLAY "Withdrawal Successful."
                           DISPLAY "New Balance: $" CHECKING-BALANCE
                       
                           MOVE CHECKING-BALANCE TO WS-TR-NEW-BALANCE
                         MOVE FUNCTION CURRENT-DATE(1:16) TO 
                                                         WS-TR-TIMESTAMP
                         MOVE 'W' TO WS-TR-TYPE    *> 'W' for Withdrawal
                         MOVE WITHDRAW-AMOUNT TO WS-TR-AMOUNT
                         MOVE USER-ID(13:4) TO WS-TR-CARD-LAST4  
                                                     *> Last 4 digits
                         DISPLAY "DEBUG: Timestamp = " WS-TR-TIMESTAMP
                         DISPLAY "DEBUG: Card Last4 = " WS-TR-CARD-LAST4
                         DISPLAY "DEBUG: Amount     = " WS-TR-AMOUNT
                         DISPLAY "DEBUG: Balance  = " WS-TR-NEW-BALANCE
                           MOVE "CHECKING" TO WS-TR-ACCOUNT-TYPE
                           IF WS-TR-TYPE = "W"
                               MOVE "WITHDRAWAL" TO WS-TYPE-DESCRIPTION
                             ELSE
                               MOVE "DEPOSIT" TO WS-TYPE-DESCRIPTION
                           END-IF
                           PERFORM LOG-TRANSACTION
                           PERFORM WRITE-RECEIPT
                           PERFORM SAVE-BALANCE
                         ELSE
                           DISPLAY "Insufficient funds in Checking." 
                           DISPLAY "Withdrawal denied."
                           MOVE "6" TO WITHDRAW-CHOICE
                       END-IF
                   
                 ELSE IF SAVINGS-BALANCE >= WITHDRAW-AMOUNT
                         COMPUTE SAVINGS-BALANCE = SAVINGS-BALANCE - 
                                                       WITHDRAW-AMOUNT
                         DISPLAY "Withdrawal Successful."
                         DISPLAY "New Balance: $" SAVINGS-BALANCE
                     
                         MOVE SAVINGS-BALANCE TO WS-TR-NEW-BALANCE
                         *> === INSERT TRANSACTION LOGGING HERE (START)
                         MOVE FUNCTION CURRENT-DATE(1:16) TO 
                                                         WS-TR-TIMESTAMP
                         MOVE 'W' TO WS-TR-TYPE    *> 'W' for Withdrawal
                         MOVE WITHDRAW-AMOUNT TO WS-TR-AMOUNT
                         MOVE USER-ID(13:4) TO WS-TR-CARD-LAST4  
                                                     *> Last 4 digits
                         DISPLAY "DEBUG: Timestamp = " WS-TR-TIMESTAMP
                         DISPLAY "DEBUG: Card Last4 = " WS-TR-CARD-LAST4
                         DISPLAY "DEBUG: Amount     = " WS-TR-AMOUNT
                         DISPLAY "DEBUG: Balance  = " WS-TR-NEW-BALANCE
                         MOVE "SAVINGS" TO WS-TR-ACCOUNT-TYPE
                           IF WS-TR-TYPE = "W"
                               MOVE "WITHDRAWAL" TO WS-TYPE-DESCRIPTION
                             ELSE
                               MOVE "DEPOSIT" TO WS-TYPE-DESCRIPTION
                           END-IF                      
                         PERFORM LOG-TRANSACTION
                         PERFORM WRITE-RECEIPT
                         PERFORM SAVE-BALANCE
                       ELSE
                         DISPLAY "Insufficient funds in Savings." 
                         DISPLAY "Withdrawal denied."
                         MOVE "6" TO WITHDRAW-CHOICE
                      END-IF
                         

         
           END-IF
           END-IF                        
           END-IF.
             
           

       CUSTOM-WITHDRAW.
           DISPLAY "Enter Custom withdrawal amount: "
           *> ACCEPT WITHDRAW-AMOUNT (we are replacing this)
           ACCEPT USER-INPUT-STR

           IF FUNCTION NUMVAL(USER-INPUT-STR) > 0
               MOVE "Y" TO NUMERIC-FLAG
               MOVE FUNCTION NUMVAL(USER-INPUT-STR) TO WITHDRAW-AMOUNT
           ELSE
               MOVE "N" TO NUMERIC-FLAG
           END-IF
       
           IF NOT-NUMERIC
             DISPLAY "Invalid input. Please enter a number."
             EXIT PARAGRAPH
           END-IF

           COMPUTE REMAINDER20 = FUNCTION MOD(WITHDRAW-AMOUNT 20)
           COMPUTE REMAINDER50 = FUNCTION MOD(WITHDRAW-AMOUNT 50)
       
           IF REMAINDER20 = 0 OR REMAINDER50 = 0
             PERFORM PROCESS-WITHDRAWAL
           ELSE
             DISPLAY "Amount must be a multiple of 20 or 50."
             DISPLAY "Try again"
           END-IF.

       
       SAVE-BALANCE.
           OPEN OUTPUT ACCOUNT-FILE
           WRITE ACCOUNT-RECORD
           CLOSE ACCOUNT-FILE.


       LOG-TRANSACTION.
           MOVE WS-TR-TIMESTAMP       TO TR-TIMESTAMP
           MOVE WS-TR-CARD-LAST4      TO TR-CARD-LAST4
           MOVE WS-TR-TYPE            TO TR-TYPE
           MOVE WS-TR-AMOUNT          TO TR-AMOUNT
           MOVE WS-TR-NEW-BALANCE     TO TR-NEW-BALANCE
           MOVE "END"                 TO TR-END-MARKER
           *> Keep this for now for easy visual check
           
           DISPLAY "DEBUG: WS-TR-DATA before MOVE: " WS-TRANSACTION-DATA
           DISPLAY "DEBUG: TRANSACTION-RECORD before"
           DISPLAY "WRITE: " TRANSACTION-RECORD
           
           OPEN EXTEND TRANSACTION-LOG
           WRITE TRANSACTION-RECORD
           CLOSE TRANSACTION-LOG
           DISPLAY "DEBUG: File Status after EXTEND/WRITE: " TRANS-STAT.

           *> *********************************************************
           *> *********************************************************

       TRANSFER-FUNDS.
           DISPLAY "Transfer FROM:"
           DISPLAY "1. Checking to Savings"
           DISPLAY "2. Savings to Checking"
           DISPLAY "Choose option (1 or 2): "
           ACCEPT TRANSFER-CHOICE

           DISPLAY "Enter amount to transfer:"
           ACCEPT TRANSFER-AMOUNT

           IF TRANSFER-CHOICE = "1"
               IF CHECKING-BALANCE >= TRANSFER-AMOUNT
                   COMPUTE CHECKING-BALANCE = CHECKING-BALANCE - 
                                                         TRANSFER-AMOUNT
                   COMPUTE SAVINGS-BALANCE = SAVINGS-BALANCE + 
                                                         TRANSFER-AMOUNT
                   DISPLAY "Transfer successful."
                   MOVE FUNCTION CURRENT-DATE(1:16) TO WS-TR-TIMESTAMP
                   MOVE 'T' TO WS-TR-TYPE
                   MOVE TRANSFER-AMOUNT TO WS-TR-AMOUNT
                   MOVE USER-ID(13:4) TO WS-TR-CARD-LAST4
                   
                   *> Determine the destination account
                   IF TRANSFER-CHOICE = "1"
                     MOVE "CHECKING" TO WS-TR-ACCOUNT-TYPE
                   ELSE
                     MOVE "SAVINGS" TO WS-TR-ACCOUNT-TYPE
                   END-IF
                   
                   MOVE "TRANSFER" TO WS-TYPE-DESCRIPTION

                   MOVE TRANSFER-AMOUNT TO WS-DISPLAY-AMOUNT

                   IF TRANSFER-CHOICE = "1"
                     MOVE CHECKING-BALANCE TO WS-DISPLAY-BALANCE
                   ELSE
                     MOVE SAVINGS-BALANCE TO WS-DISPLAY-BALANCE
                   END-IF
                   
                  IF TRANSFER-CHOICE = "1"
                        MOVE CHECKING-BALANCE TO WS-TR-NEW-BALANCE
                   ELSE
                     MOVE SAVINGS-BALANCE TO WS-TR-NEW-BALANCE
                   END-IF                  
                   
                   PERFORM LOG-TRANSACTION
                   PERFORM WRITE-RECEIPT
                   PERFORM SAVE-BALANCE

               ELSE
                   DISPLAY "Insufficient funds in Checking."
               END-IF
           ELSE IF TRANSFER-CHOICE = "2"
               IF SAVINGS-BALANCE >= TRANSFER-AMOUNT
                   COMPUTE SAVINGS-BALANCE = SAVINGS-BALANCE - 
                                                         TRANSFER-AMOUNT
                   COMPUTE CHECKING-BALANCE = CHECKING-BALANCE + 
                                                         TRANSFER-AMOUNT
                   DISPLAY "Transfer successful."
                   MOVE FUNCTION CURRENT-DATE(1:16) TO WS-TR-TIMESTAMP
                   MOVE 'T' TO WS-TR-TYPE
                   MOVE TRANSFER-AMOUNT TO WS-TR-AMOUNT
                   MOVE USER-ID(13:4) TO WS-TR-CARD-LAST4
                   
                   *> Determine the destination account
                   IF TRANSFER-CHOICE = "2"
                     MOVE "SAVINGS" TO WS-TR-ACCOUNT-TYPE
                   ELSE
                     MOVE "CHECKING" TO WS-TR-ACCOUNT-TYPE
                   END-IF
                   
                   MOVE "TRANSFER" TO WS-TYPE-DESCRIPTION

                   IF TRANSFER-CHOICE = "2"
                     MOVE SAVINGS-BALANCE TO WS-DISPLAY-BALANCE
                   ELSE
                     MOVE CHECKING-BALANCE TO WS-DISPLAY-BALANCE
                   END-IF

                  IF TRANSFER-CHOICE = "2"
                        MOVE SAVINGS-BALANCE TO WS-TR-NEW-BALANCE
                   ELSE
                     MOVE CHECKING-BALANCE TO WS-TR-NEW-BALANCE
                   END-IF                  

                   
                   PERFORM LOG-TRANSACTION
                   PERFORM WRITE-RECEIPT
                   PERFORM SAVE-BALANCE                   
               ELSE
                   DISPLAY "Insufficient funds in Savings."
               END-IF
           ELSE
               DISPLAY "Invalid transfer option."
           END-IF.
           *> *********************************************************
           *> *********************************************************


       WRITE-RECEIPT.
           *> Format the timestamp into readable date and time
           MOVE WS-TR-TIMESTAMP(1:4)   TO WS-FORMATTED-DATE(1:4)
           MOVE "/"                    TO WS-FORMATTED-DATE(5:1)
           MOVE WS-TR-TIMESTAMP(5:2)   TO WS-FORMATTED-DATE(6:2)
           MOVE "/"                    TO WS-FORMATTED-DATE(8:1)
           MOVE WS-TR-TIMESTAMP(7:2)   TO WS-FORMATTED-DATE(9:2)
       
           MOVE WS-TR-TIMESTAMP(9:2)   TO WS-FORMATTED-TIME(1:2)
           MOVE ":"                    TO WS-FORMATTED-TIME(3:1)
           MOVE WS-TR-TIMESTAMP(11:2)  TO WS-FORMATTED-TIME(4:2)
           MOVE ":"                    TO WS-FORMATTED-TIME(6:1)
           MOVE WS-TR-TIMESTAMP(13:2)  TO WS-FORMATTED-TIME(7:2)
           
           IF WS-TR-TYPE = "W"
             MOVE WITHDRAW-AMOUNT TO WS-DISPLAY-AMOUNT
           ELSE IF WS-TR-TYPE = "D"
             MOVE DEPOSIT-AMOUNT TO WS-DISPLAY-AMOUNT
           ELSE IF WS-TR-TYPE = "T"
             MOVE TRANSFER-AMOUNT TO WS-DISPLAY-AMOUNT
           END-IF
           
           MOVE WS-TR-NEW-BALANCE TO WS-DISPLAY-BALANCE

           MOVE WS-DISPLAY-AMOUNT TO WS-DISPLAY-AMOUNT-TXT
           MOVE WS-DISPLAY-BALANCE TO WS-DISPLAY-BALANCE-TXT
           
       
           OPEN OUTPUT RECEIPT-FILE
       
           MOVE "ATM RECEIPT" TO RECEIPT-LINE
           WRITE RECEIPT-LINE
       
           MOVE "------------------------------" TO RECEIPT-LINE
           WRITE RECEIPT-LINE
           
           STRING "Date/Time: " WS-FORMATTED-DATE " " WS-FORMATTED-TIME
               DELIMITED BY SIZE INTO RECEIPT-LINE
           WRITE RECEIPT-LINE
           
           MOVE SPACES TO RECEIPT-LINE
           STRING "Card ****-" WS-TR-CARD-LAST4
               DELIMITED BY SIZE INTO RECEIPT-LINE
           WRITE RECEIPT-LINE
           
           MOVE SPACES TO RECEIPT-LINE
           STRING "Transaction: " WS-TYPE-DESCRIPTION " FROM " 
           WS-TR-ACCOUNT-TYPE
             DELIMITED BY SIZE INTO RECEIPT-LINE
           WRITE RECEIPT-LINE
           
           MOVE SPACES TO RECEIPT-LINE
           STRING "Amount: $" DELIMITED BY SIZE WS-DISPLAY-AMOUNT-TXT
               DELIMITED BY SIZE
               INTO RECEIPT-LINE
           WRITE RECEIPT-LINE
           
           MOVE SPACES TO RECEIPT-LINE
           STRING "New Balance: $" WS-DISPLAY-BALANCE-TXT
               DELIMITED BY SIZE
               INTO RECEIPT-LINE
           WRITE RECEIPT-LINE
       
           MOVE "------------------------------" TO RECEIPT-LINE
           WRITE RECEIPT-LINE
       
           MOVE "THANK YOU" TO RECEIPT-LINE
           WRITE RECEIPT-LINE
       
           CLOSE RECEIPT-FILE.





           