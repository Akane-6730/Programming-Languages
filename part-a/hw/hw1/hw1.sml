(* 1. is_older:
   - fn : (int * int * int) * (int * int * int) -> bool
   - evaluates to true if the first argument is a date that comes before the second argument.
   - If the two dates are the same, the result is false *)

fun is_older (date_1 : int*int*int, date_2 : int*int*int) =
    if #1 date_1 < #1 date_2
    then true
    else if #1 date_1 = #1 date_2 andalso #2 date_1 < #2 date_2
    then true
    else if #2 date_1 = #2 date_2 andalso #3 date_1 < #3 date_2
    then true
    else false

(* 2. number_in_month:
   - fn :(int * int * int) list * int -> int
   - takes a list of dates and a month (i.e., an int)
   - returns how many dates in the list are in the given month *)

fun number_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then 0
    else if #2 (hd dates) = month
    then 1 + number_in_month (tl dates, month)
    else number_in_month (tl dates, month)

(* 3. number_in_months:
   - fn : (int * int * int) list * int list -> int
   - takes a list of dates and a list of months (i.e., an int list)
   - returns the number of dates in the list of dates that are in any of the months in the list of months *)

fun number_in_months (dates : (int * int * int) list, months : int list) =
    if null months
    then 0
    else number_in_month (dates, hd months) + number_in_months (dates, tl months)

(* 4. dates_in_month:
   - fn : (int * int * int) list * int -> (int * int * int) list
   - takes a list of dates and a month (i.e., an int)
   - returns a list holding the dates from the argument list of dates that are in the month
     The returned list should contain dates in the order they were originally given *)

fun dates_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then []
    else
        let
            val res_from_rest = dates_in_month (tl dates, month)
            val head = hd dates
        in
            if #2 head = month
            then head :: res_from_rest
            else res_from_rest
        end

(* 5. dates_in_months:
   - fn : (int * int * int) list * int list -> (int * int * int) list
   - takes a list of dates and a list of months
   - returns a list holding the dates from the argument list of dates that are in any of the months in
the list of months*)

fun dates_in_months (dates : (int * int * int) list, months : int list) =
    let
        fun append (x : (int * int * int) list, y :(int * int * int) list) =
            if null x
            then y
            else hd x :: append (tl x, y)
    in
        if null months
        then []
        else append (dates_in_month (dates, hd months), dates_in_months (dates, tl months))
    end

(* 6. get_nth
   - string list * int -> string
   - takes a list of strings and an int n
   - returns the nth element of the list where the head of the list is 1st *)

fun get_nth (strings : string list, n : int) =
    if n = 1
    then hd strings
    else get_nth (tl strings, n - 1)


(* 7. date_to_string:
   - fn : int * int * int -> string
   - takes a date and returns a string of the form January 20, 2013 (for example) *)

fun date_to_string (date : int * int * int) =
    let
        val months =  ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
       get_nth (months, #2 date) ^ " " ^ Int.toString (#3 date) ^ ", " ^ Int.toString (#1 date)
    end

(* 8. number_before_reaching_sum:
   - fn : int * int list -> int
   - takes an int called sum, which you can assume is positive
           an int list, which you can assume contains all positive numbers
   - return an int n such that
       - the first n elements of the list add to less than sum
       - but the first n + 1 elements of the list add to sum or more *)
fun number_before_reaching_sum (sum : int, list : int list) =
    if hd list >= sum
    then 0
    else number_before_reaching_sum (sum - hd list, tl list) + 1

(* 9. what_month:
   - fn : int -> int
   - takes a day of year (i.e., an int between 1 and 365)
   - returns what month that day is in (1 for January, 2 for February, etc.) *)

fun what_month (day : int) =
    let
        val dates = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        number_before_reaching_sum (day, dates) + 1
    end

(* 10. month_range:
   - fn : int * int -> int list
   - takes two days of the year day1 and day2
   - returns an int list [m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ..., and mn is the month of day day2. Note the result will have length day2 - day1 + 1 or length 0 if day1>day2 *)

fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month day1 :: month_range (day1 + 1, day2)

(* 11. oldest:
   - takes a list of dates
   - evaluates to an (int*int*int) option.
   - evaluates to NONE if the list has no dates and SOME d if the date d is the oldest date in the list *)

fun oldest (dates : (int * int * int) list) =
    if null dates
    then NONE
    else
        let
            val res_from_rest = oldest (tl dates)
        in
            if isSome res_from_rest andalso is_older (valOf res_from_rest, hd dates)
            then res_from_rest
            else SOME (hd dates)
        end
