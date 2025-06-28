(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* (a) Write a function all_except_option, which takes a string and a string list. Return NONE if the
string is not in the list, else return SOME lst where lst is identical to the argument list except the
string is not in it. You may assume the string is in the list at most once. *)

fun all_except_option (str, strs) =
    case strs of
        [] => NONE
      | s :: ss' => if same_string (s, str)
                    then SOME ss'
                    else
                        case all_except_option (str, ss') of
                            NONE => NONE
                          | SOME lst => SOME (s :: lst)

(* Write a function get_substitutions1, which takes a string list list (a list of list of strings,
the substitutions) and a string s and returns a string list. The result has all the strings that
are in some list in substitutions that also has s, but s itself should not be in the result. *)

fun get_substitutions1 (str_lsts, str) =
    case str_lsts of
      [] => []
    | str_lst :: str_lsts' => case all_except_option (str, str_lst) of
                                NONE => get_substitutions1 (str_lsts', str)
                              | SOME lst => lst @ get_substitutions1 (str_lsts', str)

(* (c) Write a function get_substitutions2, which is like get_substitutions1 except it uses a tail-recursive local helper function. *)

fun get_substitutions2 (str_lsts, str) =
    let
        fun aux (str_lsts, str, acc) =
                case str_lsts of
                    [] => acc
                  | str_lst :: str_lsts' => case all_except_option (str, str_lst) of
                                                NONE => aux (str_lsts', str, acc)
                                              | SOME lst => aux (str_lsts', str, acc @ lst)
    in
        aux (str_lsts, str, [])
    end

(* Write a function similar_names, which takes a string list list of substitutions (as in parts (b) and
(c)) and a full name of type {first:string,middle:string,last:string} and returns a list of full
names (type {first:string,middle:string,last:string} list). The result is all the full names you
can produce by substituting for the first name (and only the first name) using substitutions and parts (b)
or (c). The answer should begin with the original name (then have 0 or more other names). *)

fun similar_names (str_lsts, {first, middle, last}) =
    let
        val first_substitutions = get_substitutions2 (str_lsts, first)
        fun aux firsts =
            case firsts of
                [] => []
              | x :: xs' =>  [{first = x, middle = middle, last = last}] @ aux xs'
    in
         {first = first, middle = middle, last = last} :: aux first_substitutions
    end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)

(* (a) Write a function card_color, which takes a card and returns its color
   spades and clubs are black, diamonds and hearts are red *)

fun card_color (suit, _) =
    case suit of
        Clubs => Black | Diamonds => Red | Hearts => Red | Spades => Black

(* (b) Write a function card_value, which takes a card and returns its value
   numbered cards have their number as the value, aces are 11, everything else is 10 *)

fun card_value (_, rank) =
    case rank of
        Num x => x | Ace => 11 | _ => 10

(* (c) Write a function remove_card, which takes a list of cards cs, a card c, and an exception e.
   It returns a list that has all the elements of cs except c.
   If c is in the list more than once, remove only the first one.
   If c is not in the list, raise the exception e. You can compare cards with = *)

fun remove_card (cs, c, e) =
    case cs of
        [] => raise e
      | x :: xs' => if x = c then xs' else x :: remove_card (xs', c, e)

(* (d) Write a function all_same_color, which takes a list of cards
and returns true if all the cards in the list are the same color.  *)

fun all_same_color cs =
    case cs of
        [] => true
      | head :: [] => true
      | head :: neck :: rest => if card_color (head) = card_color (neck) andalso all_same_color (neck :: rest)
                                then true
                                else false

(* (e) Write a function sum_cards, which takes a list of cards and returns the sum of their values.
   Use a locally defined helper function that is tail recursive.
   Take "calls use a constant amount of stack space" as a requirement for this problem. *)

fun sum_cards cs =
    let
        fun aux (cs, acc) =
            case cs of
                [] => acc
              | x :: xs' => aux (xs' , acc + card_value x)
    in
        aux (cs, 0)
    end

(* (f) Write a function score, which takes a card list (the held-cards) and an int (the goal)
and computes the score as described above. *)

fun score (cs, goal) =
    let
        val sum = sum_cards cs
        val score = if sum > goal then 3 * (sum - goal) else goal - sum
    in
        if all_same_color cs
        then score div 2
        else score
    end

(* (g) Write a function officiate, which “runs a game.” It takes a card list (the card-list) a move list
(what the player “does” at each point), and an int (the goal) and returns the score at the end of the
game after processing (some or all of) the moves in the move list in order. Use a locally defined recursive
helper function that takes several arguments that together represent the current state of the game.
As described above:
    - The game starts with the held-cards being the empty list.
    - The game ends if there are no more moves. (The player chose to stop since the move list is empty.)
    - If the player discards some card c, play continues (i.e., make a recursive call) with the held-cards
      not having c and the card-list unchanged. If c is not in the held-cards, raise the IllegalMove exception.
    - If the player draws and the card-list is (already) empty, the game is over.
      Else if drawing causes the sum of the held-cards to exceed the goal, the game is over (after drawing).
      Else play continues with a larger held-cards and a smaller card-list. *)

fun officiate (cs, ms, goal) =
    let
        fun aux (cards, held_cards, moves) =
            case moves of
                [] => score (held_cards, goal)
              | Discard c :: mvs => let
                  val new_held_cards = remove_card (held_cards, c, IllegalMove)
              in
                  aux (cards, new_held_cards, mvs)
              end
              | Draw :: mvs => case cards of
                                   [] => score (held_cards, goal)
                                 | c :: cs' => let
                                     val new_held_cards = c :: held_cards
                                     val current_sum = sum_cards new_held_cards
                                 in
                                     if current_sum > goal
                                     then score (new_held_cards, goal)
                                     else aux (cs', new_held_cards, mvs)
                                 end
    in
        aux (cs, [], ms)
    end
