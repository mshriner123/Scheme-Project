;Scheme Assignment
;Skidmore College, Programming Languages, Professor Michael Eckmann
;Author: Michael Shriner
;Date: November 15, 2020


;PROBLEM: 1a)
;PARAMETER: list
;ASSUMED: list contains 0 or more numbers and no sublists 
;RETURNS list of absolute values of each of the elements
;EXAMPLE (absall1 '(1 2.3 -2 -3)) returns (1 2.3 2 3)
(define (absall1 lis)

  (cond
    ((null? lis) lis);if null, return lis
    (else (cons (abs (car lis)) (absall1 (cdr lis))))
  )
  
  )

;PROBLEM 1b)
;PARAMETER list
;ASSUMED list contains 0 or more numbers and no sublists 
;RETURNS list of absolute values of each of the elements
;DIFFERENCE between absall1 and absall2: absall2 uses map function
(define (absall2 lis)

  (cond
    ((null? lis) lis)
    (else (map abs lis)) 
  )

  )

;PROBLEM 2)
;INTENT: Inserts an atom to the left of all appearances of another atom in a list
;PARAMETERS: atm1 is the atom to insert to the left of all appearances of atm2 in lis (the list)
;ASSUMED: lis will not contain sublists
;RETURNS: The new list with atm1 inserted before every appearance of atm2 in lis
(define (multiinsertL atm1 atm2 lis)

  (cond
    ((null? lis) lis) ;is list equal to null? if yes, return lis
    ;is the first element equal to atm2? if yes, cons atm1 to car of lis and recurse on cdr of lis
    ((eq? (car lis) atm2) (cons atm1 (cons (car lis)(multiinsertL atm1 atm2 (cdr lis)))))
    (else (cons (car lis)(multiinsertL atm1 atm2 (cdr lis))));else, cons car to recursive call on cdr of lis                   
    )
  )

;PROBLEM 3)
;INTENT: Removes all occurence of an atom from a list, even if the atom is in sublist(s)
;PARAMETERS: atm: the atom to remove     lis: the list to search for atm in and remove if found
;RETURNS: lis with all occurences of atm removed
;EXAMPLE: (removeAllDeep 'a '(a b c (a a b (b a c)) wvx)) returns (b c (b (b c)) wvx)
(define (removeAllDeep atm lis)

  (cond
    ((null? lis) lis);if lis null, return lis
    ;if car of lis is a list, cons recursive call on car of lis to recursive call on cdr lis
    ((list? (car lis))(cons (removeAllDeep atm (car lis)) (removeAllDeep atm (cdr lis))))
    ;if car lis = atm, recurse on cdr of lis
    ((equal? (car lis) atm) (removeAllDeep atm (cdr lis)))
    ;else, cons car lis to recusive call on cdr of lis
    (else (cons (car lis)(removeAllDeep atm (cdr lis))))
    )
  )

;PROBLEM 4)
;INTENT: Takes in 2 lists as parameters and results in 1 list
;with the elements of the first list appearing before the elements of the second list.
;PARAMETERS: lis1 (a list) lis2 (another list)
;ASSUMPTION: Neither lis1 nor lis2 will contains sublists.
;RETURNS: A list that is a combination of lis1 and lis2 where the elements of lis1 appear before
;the elements of lis2.
;EXAMPLE: (join2Lists '(a b) '(x y z)) returns (a b x y z)
(define (join2Lists lis1 lis2)

  (cond
    ((null? lis1) lis2);if lis1 null, return lis2
    ;else, cons car of lis1 to recursive call on cdr of lis1 (leave lis 2 the same)
    (else (cons (car lis1) (join2Lists (cdr lis1) lis2)))
  )
  
  )

;PROBLEM 5)
;INTENT: Creates a list of dotted pairs from user input.
;PARAMETERS: None
;RETURNS: The list of dotted pairs created from user input.
(define (listFromInput)

  (display "enter a pair of something or q to quit")
  (newline)
  (display "enter the first item of the pair or q to quit")
  (newline)
  (let ((inp1 (read)));inp1 is the first item of the pair from user input or q
    (cond
      ((eq? inp1 'q) '());if inp1 is q, return empty list
      (else (display "enter the second item of the pair (don't enter q here)")
            (newline)
            (let ((inp2 (read)));inp2 is the second item of the pair from user input
            (cons (cons inp1 inp2)(listFromInput));cons (cons of inp1 and inp2) to a recursive call
              );end paren for second let function
            )
      );end paren for cond
   );end paren for first let function
  )

;PROBLEM 5 Cont.
;INTENT: asks for the user to input a key, searches an associative list for a pair that has that key, and
;returns the value associated with that key
;PARAMETER: lis: an associative list
;RETURNS: the value associated with the key entered by the user or "key not found" if key not found
(define (searchKey lis)

  (display "enter a key and get the value associated with that key")
  (newline)
  (let ((inp (read)));inp is the key the user entered
    (cond
      ((assoc inp lis) (cdr (assoc inp lis)));if the pair with the key was found, return the associated value
      (else (display "key not found"));else, display "key not found"
      )
    );end paren of let function

  )

;PROBLEM 5 Cont.
;INTENT: Calls listFromInput, and sends its return value as a parameter to searchKey
;PARAMETERS: None
;RETURNS: The value associated with the key that the user entered in searchKey function.
(define (enterAndSearch)
  (searchKey (listFromInput))
  )

;PROBLEM 6)
;INTENT: Returns the last element of the list if called with list alone as a paramter. Else,
;returns a list of the last number elements of the list (see example below). If the optional paramter
;is a number greater then or equal to the length of the list, returns the whole list. If optional paramter
;equals 0, returns empty list. If list is empty, returns empty list.
;PARAMETER(S); lis: a list    optparms: a list of optional paramters
;ASSUMED: optparms is only one parameter if a parameter at all, and optparms is a number if a paramter
;RETURNS:
;empty list iff (optional paramter = 0 or lis = '())
;last element of lis iff (optional parameter = 1 (in which case, returns a list) or no optional paramter or lis of size 1)
;list of n number of atoms from the end of lis where n = optional param iff optional param <= length of lis
;lis iff (optional param >= length of lis or lis of size 1)
;EXAMPLES:
;(last '(1 2 3) '2) returns (2 3)
;(last '(1 2 3)) returns 3
(define (last lis . optparms)

  (cond
    ((null? lis) lis);if lis null, return lis 
    ((null? optparms) (list-ref lis (- (length lis) 1)));if no optional paramter, return the last element of the list
    ((<= (car optparms) 0)'());if the optional parameter is <= 0, return empty list
    ((>= (car optparms) (length lis)) lis);if the optional parameter >= length of lis, return lis
    ;else, cons atom at index (length lis - optional param) of lis to a recursive call with optional param - 1
    (else (cons (list-ref lis (-(length lis) (car optparms))) (last lis (- (car optparms) 1))))
  )
  )

;PROBLEM 7)
;INTENT: Adds all of the corresponding numbers in two lists and returns the resulting list.
;If the lists have unequal length, then the resulting list has the rest of the larger list appended to it.
;PARAMETERS: lis1: a list   lis2: a list
;RETURNS: a list with all of the corresponding numbers in lis1 and lis2 summed together
;EXAMPLES:
;(addCorresponding '(1 2 3) '(4 5 6)) returns (5 7 9)
;(addCorresponding '(1 2 3) '(4)) returns (5 2 3)    
(define (addCorresponding lis1 lis2)

  (cond
    ((null? lis1) lis2);if lis1 null, return lis2
    ((null? lis2) lis1);if lis2 null, return lis1
    ;else, cons the sum of the car of lis1 and car of lis2 to a recursive call on the cdr of each list
    (else (cons (+ (car lis1)(car lis2))(addCorresponding (cdr lis1)(cdr lis2))))
    )
  )

;PROBLEM 8. What would be a better name for this function?  What does it result in?

  (define (func n m)
    (cond 
      ((< n m) 0)
      (else (+ 1 (func (- n m) m)))))

;A better name for this function is divide because it results in n/m where the result is truncated (e.g., n = 5 and m = 2 returns
;2). Any numerator less than the denominator is truncated so that it returns 0. Any numerator greater than the denominator returns
;1 * the number of times n is divisible by m.


;PROBLEM 9) For the following list: '((a b c (q r s)) (t t) (d e f) (g (h i) j) (y z))

;a) What sequence of cdr's and car's are necessary to get the atom i to be returned?

;(car(cdr (car (cdr (car ( cdr ( cdr ( cdr lis)))))))) returns i

;b) Write out the code to do it.

(define (returnI lis)

  (car(cdr (car (cdr (car ( cdr ( cdr ( cdr lis))))))))

  )

;PROBLEM 10) (below is one of three functions)
;INTENT: Returns the largest positive and odd integer or real number less than the paramter number or '() if the paramter
;number is <= 1.
;PARAMETER: num: integer or real number.
;RETURNS: The largest odd integer less than num
;EXAMPLES:
;(largestOddLessThan '6) returns 5
;(largestOddLessThan '7.2) returns 7.0
(define (largestOddLessThan num)

  (cond
    ((<= num 1) '());if num <= 1, return empty list
    ((even? (floor num))(- (floor num) 1));check if even or real or both, and if true, return (floor of num - 1)
    ((integer? num) (if (odd? num) (- num 2)));check if integer (7.0 counts as integer) and odd, and if true, return (num -2)
    (else (floor num));if here, we know it is odd and real, return its floor         
    )
  )

;PROBLEM 10 Cont.
;INTENT: Returns a list containing all of the odd integers from start to the end paramter.
;PARAMETER: start = start of list    end = end of list
;ASSUMED: end is an odd integer and start = 1
;RETURNS: a list of all of the odd integers from start to end
;EXAMPLE: (oddList 1 5) returns 1 3 5
(define (oddList start end)

  (cond
    ((> start end) '()) ;if start > end, return '()
    ;else, cons start to a recursive call with (start + 2)
    (else (cons start (oddList (+ start 2) end)))
  )
  )

;PROBLEM 10 Cont.
;INTENT: Takes one parameter which is a number (real or integer) and returns a list containing all the
;non-negative odd integers strictly less than that number in increasing order. If the parameter number is less
;than or equal to 1, returns empty list. 
;PARAMETER: num: a real number or an integer
;RETURNS: A list of odd integers < the paramter number sorted in increasing order or the empty list. 
;EXAMPLE: (odd-list '5) returns (1 3)
(define (odd-list num)
  (cond
    ((<= num 1) '())
    (else (oddList 1 (largestOddLessThan num)))
    )  
  )

;;;;;;;;;;;;;;; THE FOLLOWING FUNCTIONS SOLVE PROBLEMS 11, 12, and 13 ;;;;;;;;;;;;;;;;;;;;;;;;


;INTENT: Calls getHandVal with a blackjack hand and an associated list of cards with values and returns the blackjack hand's
;total value
;PARAMTER: hand: a blackjack hand as a list (e.g., jack ace queen)
;RETURNS: The value of hand. 
(define (blackjackValue hand)
  ;getHandVal returns the hand's value
  ;createAssocLis returns an associative list, which is used as a parameter to getHandVal
  (getHandVal hand (createAssocLis '(ace 1 two 2 three 3 four 4 five 5 six 6 seven 7 eight 8 nine 9 ten 10 jack 10 queen 10 king 10)))
)

;INTENT: Returns the value of a blackjack hand sent in as a parameter.
;PARAMETERS: hand: a blackjack hand as a list     valueLis: an associative list of cards and their values
;RETURNS: The value of the blackjack hand. 
(define (getHandVal hand valueLis)
  ;calls add-non-ace to return the sum of all non ace cards
  ;returns that sum and sends it as a parameter to add all ace cards if any exist using addAces function
  (addAces hand valueLis (add-non-ace hand valueLis));returns the value of the blackjack hand

  )

;INTENT: Adds all non-ace cards, if any exist, in the paramter blackjack hand.
;PARAMETERS: hand: a blackjack hand as a list     valueLis: an associative list of cards and their values
;RETURNS: The sum of the values of all non-ace cards in hand. 
(define (add-non-ace hand valueLis)

  (cond
    ((null? hand) 0);if hand is empty, return 0
    ;if the card value > 1, not ace, add its value to a recursive call on cdr of hand
    ;note, valueLis only contains ace with the associated value 1
    ((> (cdr (assoc (car hand) valueLis)) 1) (+ (cdr (assoc (car hand) valueLis)) (add-non-ace (cdr hand) valueLis)))
    (else (+ 0 (add-non-ace (cdr hand) valueLis)));else, card is an ace, add 0 to a recursive call on cdr of hand
    )

  )

;INTENT: Adds the values of ace card(s) if any exist in the blackjack hand to the current value of the hand, which was
;returned by add-non-ace.
;PARAMTERS:
;hand: a blackjack hand as a list
;valueLis: an associative list of cards and their values
;currVal: the value of the blackjack hand excluding the value of any ace card(s)
;RETURNS: currVal = the sum of all non-ace cards and all ace-cards 
(define (addAces hand valueLis currVal)

  ;ACE RULES:
  
  ;when the hand has one ace, count it as 11 as long as the whole hand value is 21 or less
  ;otherwise, count it as 1

  ;when the hand has more than one ace: only one of the aces can count as either 11 or 1, all other 
  ;aces must count as 1. So, count one ace as 11 and all others as 1 if the whole 
  ;hand value is 21 or less. However, if that results in a whole hand value of 22 or 
  ;more, then count all aces as 1.

  (cond
    ((null? hand) currVal);if hand null, return currVal, all cards have been summed
    ;if current card is an ace, add 11 to currVal if currVal <= 10 or <= 9 and recurse on cdr of hand 
    ((= (cdr (assoc (car hand) valueLis)) 1)
     (if (or (<= currVal 10) (<= currVal 9))
         (addAces (cdr hand) valueLis (+ currVal 11))
         (addAces (cdr hand) valueLis (+ currVal 1))
         )
     )
    ;else, current card not an ace, recurse of cdr of hand, currVal stays the same
     (else (addAces (cdr hand) valueLis currVal))
    )

)

;INTENT: Returns an associative list, created from the parameter list where each pair of indices in the paramter
;list are treated as a pair of keys and values.
;ASSUMPTION: For current use, lis = (ace 1 two 2 three 3 four 4 five 5 six 6 seven 7 eight 8 nine 9 ten 10 jack 10 queen 10 king 10)
;PARAMETER: lis: the list as described in the assumption
;RETURNS: an associative list created from lis
;EXAMPLE: (createAssocLis '(ace 1 two 2 three 3 four 4 five 5 six 6 seven 7 eight 8 nine 9 ten 10 jack 10 queen 10 king 10))
;returns ((ace . 1) (two . 2) (three . 3) (four . 4) (five . 5) (six . 6) (seven . 7) (eight . 8) (nine . 9) (ten . 10)
;(jack . 10) (queen . 10) (king . 10))
(define (createAssocLis lis)

  (cond
    ((null? lis) lis);if null, return '()
    ;otherwise, cons first two cars together and cons that to the rest of the list
    (else (cons (cons (car lis) (car (cdr lis))) (createAssocLis (cdr(cdr lis)))))
    )

  )

;INTENT: Reads 2 blackjack cards from the user, displays their total value (as a hand),
;displays the hand, suggests whether to hit or stay, and calls a method to allow the user
;to hit or stay.
;PARAMETER: None
(define (startBlackJackGame)

  (display "Enter a blackjack hand (enter one card per input box):")
  (newline)
  (let ((card1 (read)));card1 is the first card of the hand
    (let((card2 (read)));card2 is the second card of the hand
      (displayValAndHand (cons card1 (cons card2 '())));function to display hand's value and the hand itself
      (hitStaySuggestion (blackJackValue (cons card1 (cons card2 '()))));function to suggest hit or stay
      (hitStay (cons card1 (cons card2 '())));function to allow the user to hit or stay
      )
    )
  )

;INTENT: Displays the value of the blackjack hand sent in as a parameter and the hand itself.
;PARAMETER: hand: user's blackjack hand as a list
(define (displayValAndHand hand)
  (display (blackJackValue hand));displays value of blackjack hand
  (newline)
  (display hand);displays the blackjack hand as a list 
  (newline)
  )
    
;INTENT: Allows the user to hit or stay. If the user stays, the game ends. If
;the user reaches a total value greater than 21, the game ends. When the game ends,
;the user's hand value and hand are printed.
;PARAMETER: hand: user's blackjack hand
(define (hitStay hand)

  (display "Enter a card or stay to quit:")
  (newline)

  (let ((inp (read)));inp is either a card or "stay"
    (cond
      ((eq? inp 'stay) (displayValAndHand hand));if inp equals stay, end game
      (( <= (blackJackValue (cons inp hand)) 21);if hand value <= 21, add new card to hand
      (displayValAndHand (cons inp hand));displays value of new hand and the new hand
      (hitStaySuggestion (blackJackValue (cons inp hand)));suggests whether to hit or stay
      (hitStay (cons inp hand));recurses with new hand
      )
      ;else, hand value > 21, end game
      (else (displayValAndHand (cons inp hand)))     
      )
    );end paren of let function
  )

;INTENT: Display a suggestion to either hit or stay. If the blackjack hand's value is less than
;or equal to 16, suggest hit. Else, suggest stay.
;PARAMETER: sumCards: the value of the user's blackjack hand.
(define (hitStaySuggestion sumCards)

  (cond
    ((<= sumCards 16)(display "It is suggested that you HIT") (newline))
    (else (display "It is suggested that you STAY") (newline))
  )
)
    
