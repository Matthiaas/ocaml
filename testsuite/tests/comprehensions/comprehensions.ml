(* TEST
   * expect
*)


(*Type checking tests.*)

true::[i for i = 10 downto 0];;
[%%expect{|
Line 1, characters 7-8:
1 | true::[i for i = 10 downto 0];;
           ^
Error: This expression has type int but an expression was expected of type
         bool
|}];;


module M = struct type t = A | B end;;
let x : M.t list  = [A for i = 1 to 1];;
[%%expect{|
module M : sig type t = A | B end
val x : M.t list = [M.A]
|}];;

[A for i = 1 to 1];;
[%%expect{|
Line 1, characters 1-2:
1 | [A for i = 1 to 1];;
     ^
Error: Unbound constructor A
|}];;

M.B::[A for i = 1 to 1];;
[%%expect{|
- : M.t list = [M.B; M.A]
|}, Principal{|
Line 1, characters 6-7:
1 | M.B::[A for i = 1 to 1];;
          ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not principal.
- : M.t list = [M.B; M.A]
|}];;


let y = 10;;
[i for i in y];;
[%%expect{|
val y : int = 10
Line 2, characters 12-13:
2 | [i for i in y];;
                ^
Error: This expression has type int but an expression was expected of type
         'a list
       because it is in the iteration argument of a comprehension
|}];;

let y = [1;2;3];;
true::[i for i in y];;
[%%expect{|
val y : int list = [1; 2; 3]
Line 2, characters 7-8:
2 | true::[i for i in y];;
           ^
Error: This expression has type int but an expression was expected of type
         bool
|}];;


let y = [[1]];;
true::[i for i in z for z in y];;
[%%expect{|
val y : int list list = [[1]]
Line 2, characters 7-8:
2 | true::[i for i in z for z in y];;
           ^
Error: This expression has type int but an expression was expected of type
         bool
|}];;

let y = [[]];;
[i for i in z and z in y];;
[%%expect{|
val y : 'a list list = [[]]
Line 2, characters 12-13:
2 | [i for i in z and z in y];;
                ^
Error: Unbound value z
|}];;

(*List construction tests.*)

[i for i = 0 to 10];;
[%%expect{|
- : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
|}];;


[i for i = 10 downto 0];;
[%%expect{|
- : int list = [10; 9; 8; 7; 6; 5; 4; 3; 2; 1; 0]
|}];;


let y = [1;2;3];;
[i for i in y];;
[%%expect{|
val y : int list = [1; 2; 3]
- : int list = [1; 2; 3]
|}];;

let y = [0;1;2;3];;
[ (k*4*4 + j*4 + i) for i in y for j in y for k in y];;
[%%expect{|
val y : int list = [0; 1; 2; 3]
- : int list =
[0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20;
 21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31; 32; 33; 34; 35; 36; 37; 38; 39;
 40; 41; 42; 43; 44; 45; 46; 47; 48; 49; 50; 51; 52; 53; 54; 55; 56; 57; 58;
 59; 60; 61; 62; 63]
|}];;


let y = [0;1;2;3];;
[ (k*4*4 + j*4 + i) for i in y and j in y and k in y];;
[%%expect{|
val y : int list = [0; 1; 2; 3]
- : int list =
[0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20;
 21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31; 32; 33; 34; 35; 36; 37; 38; 39;
 40; 41; 42; 43; 44; 45; 46; 47; 48; 49; 50; 51; 52; 53; 54; 55; 56; 57; 58;
 59; 60; 61; 62; 63]
|}];;

(*Array construction tests*)

let y = [|0;1;2;3|];;
[| (k*4*4 + j*4 + i) for i in y for j in y for k in y |];;
[%%expect{|
val y : int array = [|0; 1; 2; 3|]
- : int array =
[|0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20;
  21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31; 32; 33; 34; 35; 36; 37; 38; 39;
  40; 41; 42; 43; 44; 45; 46; 47; 48; 49; 50; 51; 52; 53; 54; 55; 56; 57; 58;
  59; 60; 61; 62; 63|]
|}];;

let y = [|0;1;2;3|];;
[| (k*4*4 + j*4 + i) for i in y and j in y and k in y|];;
[%%expect{|
val y : int array = [|0; 1; 2; 3|]
- : int array =
[|0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20;
  21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31; 32; 33; 34; 35; 36; 37; 38; 39;
  40; 41; 42; 43; 44; 45; 46; 47; 48; 49; 50; 51; 52; 53; 54; 55; 56; 57; 58;
  59; 60; 61; 62; 63|]
|}];;

let y = [|0;1;2;3|];;
[| (k*4*4 + j*4 + i) for i in y for j in y and k in y|];;
[%%expect{|
val y : int array = [|0; 1; 2; 3|]
- : int array =
[|0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20;
  21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31; 32; 33; 34; 35; 36; 37; 38; 39;
  40; 41; 42; 43; 44; 45; 46; 47; 48; 49; 50; 51; 52; 53; 54; 55; 56; 57; 58;
  59; 60; 61; 62; 63|]
|}];;

let y = [|0;1;2;3|];;
[| (k*4*4 + j*4 + i) for i in y and j in y for k in y|];;
[%%expect{|
val y : int array = [|0; 1; 2; 3|]
- : int array =
[|0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20;
  21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31; 32; 33; 34; 35; 36; 37; 38; 39;
  40; 41; 42; 43; 44; 45; 46; 47; 48; 49; 50; 51; 52; 53; 54; 55; 56; 57; 58;
  59; 60; 61; 62; 63|]
|}];;




[| (k*4*4 + j*4 + i) for i = 0 to 3 and j = 0 to 3  for k = 0 to 3 |];;
[%%expect{|
- : int array =
[|0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20;
  21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31; 32; 33; 34; 35; 36; 37; 38; 39;
  40; 41; 42; 43; 44; 45; 46; 47; 48; 49; 50; 51; 52; 53; 54; 55; 56; 57; 58;
  59; 60; 61; 62; 63|]
|}];;


[| (float_of_int (k*4*4 + j*4 + i)) for i = 0 to 3 and j = 0 to 3  for k = 0 to 3 |];;
[%%expect{|
- : float array =
[|0.; 1.; 2.; 3.; 4.; 5.; 6.; 7.; 8.; 9.; 10.; 11.; 12.; 13.; 14.; 15.; 16.;
  17.; 18.; 19.; 20.; 21.; 22.; 23.; 24.; 25.; 26.; 27.; 28.; 29.; 30.; 31.;
  32.; 33.; 34.; 35.; 36.; 37.; 38.; 39.; 40.; 41.; 42.; 43.; 44.; 45.; 46.;
  47.; 48.; 49.; 50.; 51.; 52.; 53.; 54.; 55.; 56.; 57.; 58.; 59.; 60.; 61.;
  62.; 63.|]
|}];;


let y = [| [| [| 1;2;|]; [| 3;4; |] |]; [| [| 5;6;|]; [| 7;8; |] |] |];;
[| i for i in x for x in z for z in y |];;
[%%expect{|
val y : int array array array =
  [|[|[|1; 2|]; [|3; 4|]|]; [|[|5; 6|]; [|7; 8|]|]|]
- : int array = [|1; 2; 3; 4; 5; 6; 7; 8|]
|}];;

let y = [| [| [| 1;2;|]; [| 3;4; |] |]; [| [| 5;6;|]; [| 7;8; |] |] |];;
[| (i,j) for i in x and j in x for x in z for z in y |];;
[%%expect{|
val y : int array array array =
  [|[|[|1; 2|]; [|3; 4|]|]; [|[|5; 6|]; [|7; 8|]|]|]
- : (int * int) array =
[|(1, 1); (2, 1); (1, 2); (2, 2); (3, 3); (4, 3); (3, 4); (4, 4); (5, 5);
  (6, 5); (5, 6); (6, 6); (7, 7); (8, 7); (7, 8); (8, 8)|]
|}];;

let y = [| [| [| 1;2;|]; [| 3;4; |] |]; [| [| 5;6;|]; [| 7;8; |] |] |];;
[| (string_of_int i,j) for i in x and j in x for x in z for z in y |];;
[%%expect{|
val y : int array array array =
  [|[|[|1; 2|]; [|3; 4|]|]; [|[|5; 6|]; [|7; 8|]|]|]
- : (string * int) array =
[|("1", 1); ("2", 1); ("1", 2); ("2", 2); ("3", 3); ("4", 3); ("3", 4);
  ("4", 4); ("5", 5); ("6", 5); ("5", 6); ("6", 6); ("7", 7); ("8", 7);
  ("7", 8); ("8", 8)|]
|}];;
