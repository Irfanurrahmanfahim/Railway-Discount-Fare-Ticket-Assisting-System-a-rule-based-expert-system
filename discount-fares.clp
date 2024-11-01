;; Function to welcome the user & accept user's full name as input
(deffunction start_system ()
  (clear-window)
  (printout t "Welcome to the Discount Fares System" crlf crlf)
  (printout t "Use this expert system to determine the best discount available for your train ticket purchase." crlf crlf)
  (printout t "Hi, What is your first name?" crlf)
  (bind ?username (read))       ;; accepting user's full name as input
  (assert (user_fullname ?username)))
 



 ;; Function to accept & validate yes/no input from the user
(deffunction input_yes_no (?val)
 (while TRUE
     (printout t ?val crlf)
     (bind ?userinput (read))    ;; accepting user input

     (if (or (eq ?userinput yes) (eq ?userinput no))
      then
        (return ?userinput)  ;; Return yes/no input only
      else
        (printout t "Invalid input. Please enter 'yes' or 'no'." crlf)
    )
  )
)
  
  
  
  
  
 ;; Function to display the menu options at the end of each session
  (deffunction session-menu ()
  (while TRUE
    (printout t crlf "Choose an option:" crlf)
    (printout t "1. Show All Facts" crlf)
    (printout t "2. Reset and Restart" crlf)
    (printout t "3. Close Session" crlf)
    (printout t "Enter your choice (1, 2, or 3): " crlf)
    (bind ?choice (read))    ;; accepting user input

    (if (eq ?choice 1)
        then
		  (printout t crlf "Current facts:" crlf)
          (facts)  ;; Display all facts 
          
      else
        (if (eq ?choice 2)
            then
              (reset)  ;; Clears all facts and rules
              (printout t "System reset." crlf)
              (return) 
			  
          else
            (if (eq ?choice 3)
                then
                   (printout t "Closing session." crlf)
                   (exit)  ;; Closes the CLIPSIDE application

              else
                (printout t "Invalid choice. Please enter '1', '2', or '3'." crlf)
            )
        )
    )
  )
)
  
  
  
 
 ;; Rule to check if you are a regular commuter
  (defrule inquiry_regular-commuter
  (not (regular_commute ?commute))
  =>
  (start_system)    ;; calling 'start_system' function to start the expert system
   (bind ?commute (input_yes_no "Are you a regular commuter (yes/no)?"))    ;; calling 'input_yes_no' function to accept & validate user's response
  (assert (regular_commute ?commute))
  ?commute)

  
  
  
  
  ;;Rule to check if the user travels daily
  (defrule inquiry_dailyuser
  (not (dailyuser ?daily))
  (regular_commute yes)
  =>
   (bind ?daily (input_yes_no "Do you travel daily (yes/no)?"))   ;; calling 'input_yes_no' function to accept & validate user's response  
  (assert (dailyuser ?daily))
  ?daily)
  
  
  
  
  
   ;;Rule to check if the user has a salary sacrifice scheme & suggest tickets based on that information
  (defrule inquiry_salaryscheme
  (not (salaryscheme ?scheme))
  (dailyuser yes)
  =>
   (bind ?scheme (input_yes_no "Do you have a salary sacrifice scheme (yes/no)?"))    ;; calling 'input_yes_no' function to accept & validate user's response
  (assert (salaryscheme ?scheme))
  ?scheme
  
  (if (eq ?scheme yes)  ;;show suitable discount options based on user's response
      then
        (printout t "Season Ticket via your employer will be the best option for you as you have a salary sacrifice scheme." crlf)  
		(session-menu)   ;; calling 'session-menu' function to display the menu options at the end
      else
        (printout t "Season Ticket will be the best option for you as you do not have a salary sacrifice scheme." crlf)
		(session-menu)   ;; calling 'session-menu' function to display the menu options at the end
    ))
  
 
  
  
  
   ;;Rule to check if the user makes other journeys
  (defrule inquiry_otherjourney
  (not (otherjourney ?other))
  (dailyuser no)
  =>
   (bind ?other (input_yes_no "Do you make other journeys (yes/no)?"))   ;; calling 'input_yes_no' function to accept & validate user's response
  (assert (otherjourney ?other))
  ?other
  
  (if (eq ?other yes)   ;;show suitable discount option based on user's response
      then
        (printout t "Check Network Railcard options before buying your ticket to get up to 1/3 off most standard adult rail fares." crlf) 
		(session-menu)))   ;; calling 'session-menu' function to display the menu options at the end
  
  
  
  
  
  
   ;;Rule to check if the user can travel outside peak time & suggest tickets based on that information
  (defrule inquiry_peaktime
  (not (peaktime ?peak))
  (otherjourney no)
  =>
   (bind ?peak (input_yes_no "Can you travel outside peak time (yes/no)?"))    ;; calling 'input_yes_no' function to accept & validate user's response
  (assert (peaktime ?peak))
  ?peak
  
  
  (if (eq ?peak yes)   ;;show suitable discount options based on user's response
      then
        (printout t "Saver or Super Saver Ticket will be the best option for you as you can travel outside peak time." crlf)
        (session-menu)   ;; calling 'session-menu' function to display the menu options at the end
      else
        (printout t "Standard Ticket will be the best option for you as you cannot travel outside peak time." crlf)
		(session-menu)   ;; calling 'session-menu' function to display the menu options at the end
    ))
  
  
  
  
  
 ;;Rule to check if the user travels in a group
  (defrule inquiry_travelgroup
  (not (travelgroup ?group))
  (regular_commute no)
  =>
   (bind ?group (input_yes_no "Do you travel in a group (yes/no)?"))   ;; calling 'input_yes_no' function to accept & validate user's response
  (assert (travelgroup ?group))
  ?group)
  
  
  
  
  
  ;;Rule to check if the user wants ticket for a single journey with the group
  (defrule inquiry_singlegroupjourney
  (not (singlegroupjourney ?single))
  (travelgroup yes)
  =>
   (bind ?single (input_yes_no "Do you want ticket for a single journey with this group (yes/no)?"))   ;; calling 'input_yes_no' function to accept & validate user's response
  (assert (singlegroupjourney ?single))
  ?single
  
  (if (eq ?single yes)   ;;show suitable discount option based on user's response
      then
        (printout t "The Group Saver Ticket is the ideal choice for you since you’re traveling with a group on a single journey. Check validity according to your journey." crlf)
		(session-menu))    ;; calling 'session-menu' function to display the menu options at the end
   )
  
  
  
  
  ;;Rule to check if the user is travelling with friend/companion or family and suggesting tickets based on that information
  (defrule inquiry_familyorcompanion
  (not (familyorcompanion ?familyorfriend))
  (singlegroupjourney no)
  =>
  (printout t crlf "Please select the option below that best describes your journey: " crlf)
  (printout t "1. Travelling with your family." crlf)
  (printout t "2. Travelling with your friend or companion." crlf)
    
  (while TRUE      ;; Loop continues until a valid input is entered
   (bind ?familyorfriend (read))
  (assert (familyorcompanion ?familyorfriend))
  ?familyorfriend
  

    (if (or (eq ?familyorfriend 1) (eq ?familyorfriend 2))
      then
        (printout t ?familyorfriend crlf)  ;; Return the valid input
		 (if (eq ?familyorfriend 1) 
      then
        (printout t "Tickets with Family Railcard will be the best option for you as you are travelling with your family." crlf)
		(session-menu)   ;; calling 'session-menu' function to display the menu options at the end
		(return)
       else  
     (printout t "Tickets with Two Together Railcard will be the best option for you as you are travelling with your friend/companion." crlf)
	 (session-menu)   ;; calling 'session-menu' function to display the menu options at the end
		(return)
      else
        (printout t "Invalid input. Please enter '1' or '2'." crlf)
    )
  )	
  )
  )
  
  
  
  
  ;;Rule to check if the user is a regular traveller
  (defrule inquiry_regulartraveller
  (not (regulartraveller ?regular))
  (travelgroup no)
  =>
   (bind ?regular (input_yes_no "Are you a regular traveller (yes/no)?"))   ;; calling 'input_yes_no' function to accept & validate user's response
  (assert (regulartraveller ?regular))
  ?regular)
  
  
   ;;Rule to check user's age and suggest a railcard according to age
  (defrule inquiry_agerailcard
  (not (agerailcard ?age))
  (regulartraveller yes)
  =>
  (printout t crlf "Please select your age category from the options below:" crlf)
  (printout t "1. 16 to 25" crlf)
  (printout t "2. 26 to 30" crlf)
  (printout t "3. 60+" crlf)
   
  
  
  (while TRUE      ;; Loop continues until a valid input is entered
   (bind ?age (read))
  (assert (agerailcard ?age))
  ?age
  
    (if (or (eq ?age 1)(eq ?age 2)(eq ?age 3))
      then
        (printout t ?age crlf)  
		 (if (eq ?age 1) 
      then
        (printout t "Tickets with Young Persons Railcard are the best option for you since you fall within the 16 to 25 age category." crlf)
		(session-menu)   ;; calling 'session-menu' function to display the menu options at the end
		(return))
       (if (eq ?age 2)  
	   then
     (printout t "Tickets with 26-30 Railcard are the best option for you since you fall within the 26 to 30 age category." crlf)
	 (session-menu)     ;; calling 'session-menu' function to display the menu options at the end
	 (return))
	 (if (eq ?age 3)  
	   then
     (printout t "Tickets with Senior Citizens Railcard will be the best option for you since you are in the 60+ age category." crlf)
	 (session-menu)    ;; calling 'session-menu' function to display the menu options at the end
	 (return))
      else
        (printout t "Invalid input. Please enter '1','2' or '3'." crlf)
    )
  ))
  
  
  
  
   ;;Rule to check if the user is going on a holiday
  (defrule inquiry_holiday
  (not (holidaytravel ?holiday))
  (regulartraveller no)
  =>
   (bind ?holiday (input_yes_no "Are you going for a holiday (yes/no)?"))      ;; calling 'input_yes_no' function to accept & validate user's response
  (assert (holidaytravel ?holiday))
  ?holiday
  
  (if (eq ?holiday yes) 
      then
        (printout t "Check for '5 from 7' network rider availability for discount" crlf)
		else
		(printout t "No discounts available for tickets." crlf)
	) 
		(session-menu)   ;; calling 'session-menu' function to display the menu options at the end
    )