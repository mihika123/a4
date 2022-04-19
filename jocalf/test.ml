open OUnit2
open Ast
open Interp

(* All of these tests will currently fail because you have not yet
   implemented interpretation of any of these syntactic forms. A
   completed interpreter should pass all of them, though. OCaml allows
   {|...|} as a syntax for strings in which the ... can contain
   unescaped quotes. This is super useful for constructing test cases,
   as shown below. *)
let tests =
  [
    ("int constant", {|42|}, "42");
    ("true", {|true|}, "true");
    ("false", {|false|}, "false");
    ("string constants", {|"xyzzy"|}, {|"xyzzy"|});
    ("undefined", {|undefined|}, "undefined");
    ("plus 1 1", {|1+1|}, "2");
    ("add booleans", {|false+true|}, "1");
    ("add strings", {|"hello"+"43"|}, {|"hello43"|});
    ("add a string and an int", {|"hello"+43|}, {|"hello43"|});
    ("and of booleans", {|false && true|}, "false");
    ("and of booleans 2", {|true && false|}, "false");
    ("and of booleans 3", {|true && true|}, "true");
    ("and of booleans 4", {|false && false|}, "false");
    ("and of boolean and int", {|0 && false|}, "0");
    ("and of string and int", {|"okay" && "boomer"|}, {|"boomer"|});
    ("or of booleans", {|false || true|}, "true");
    ("or of booleans 2", {|true ||
       false|}, "true");
    ("or of booleans 3", {|true || true|}, "true");
    ("or of booleans 4", {|false || false|}, "false");
    ("or of boolean and int", {|0 || true|}, "true");
    ("and of string and int", {|"" && ""|}, {|""|});
    ("let var", {|let x = 0 in 0|}, "0");
    ("let var 2", {|let x = 0 in x|}, "0");
    ( "unbound var",
      {|let x = 0 in y|},
      {|Exception: "Error: Unbound variable"|} );
    ("anonymous function", {|fun (x) -> 0|}, "<closure>");
    ( "apply non-function",
      {|0 0|},
      {|Exception: "Error: Not a function"|} );
    ( "apply wrong arity",
      {|(fun (x) -> 0) 1 2|},
      {|Exception: "Error: Wrong number of arguments"|} );
    ("test basic if", {|if true then "yay" else "nay"|}, {|"yay"|});
    ( "test if with numbers in conditional",
      {|if 0 then 7 else 11|},
      "11" );
    ("Test if partial", {|if true then 7|}, "7");
    ("Test if partial for undefined", {|if false then 0|}, "undefined");
    ("test for sequences", {|7; if false then 5|}, "undefined");
    ("test for sequences 2", {|3; 8 + 9|}, "17");
    ("ref", {|ref 0|}, "<location>");
    ( "assign non location",
      {|1 := 0|},
      {|Exception: "Error: Assignment to non-location"|} );
    (* ( "function app + ref", {|let inc = fun (r) -> r := !r + 1|},
       "<closure>" ); *)
    ("test referencing and assignment", {|let x = ref 0 in x:=10|}, "10");
    ("throw", {|throw 0|}, "Exception: 0");
    ( "try and throw",
      {|try throw "oops" catch exc handle exc + " caught"|},
      {|"oops caught"|} );
    ( "finally test",
      {|try throw 1 catch x handle throw 3 finally throw 2|},
      "Exception: 2" );
    ("div by 0", {|4/0|}, {|Exception: "Error: Division by zero"|});
    ("mod by 0", {|4 mod 0|}, {|Exception: "Error: Division by zero"|});
    ("string of operations", {|(1 + 2) / (6 - (3))|}, "1");
    ("comparison operation 1", {|"hello" > "hello"|}, "false");
    ("comparison operation 2", {|"hello" > "hel"|}, "true");
    ("comparison operation 3", {|1>0|}, "true");
    ("comparison operation 4", {|false<3|}, "true");
    ("comparison operation 5", {|"oh">=19|}, "false");
    ("physical equality operation 1", {|undefined = undefined|}, "true");
    ("physical equality operation 2", {|"hello" = "hel"|}, "false");
    ("physical equality operation 3", {|1 = 10 - 9|}, "true");
    ("physical equality operation 4", {|true = not false|}, "true");
    ("physical equality operation 5", {|false = "0"|}, "false");
    ("physical equality operation 6", {|false = 0|}, "true");
    ( "physical inequality operation 1",
      {|undefined != undefined|},
      "false" );
    ("physical inequality operation 2", {|"hello" != "hel"|}, "true");
    ("physical inequality operation 3", {|1 != 10 - 9|}, "false");
    ("physical inequality operation 4", {|true != not false|}, "false");
    ("physical inequality operation 5", {|false != "0"|}, "true");
    ("physical inequality operation 6", {|false != 0|}, "false");
    ( "structural equality operation 1",
      {|undefined == undefined|},
      "true" );
    ("structural equality operation 2", {|"hello" == "hel"|}, "false");
    ("structural equality operation 3", {|1 == 10 - 9|}, "true");
    ( "structural inequality operation 4",
      {|true !== not false|},
      "false" );
    ("structural inequality operation 5", {|false !== "0"|}, "true");
    ("structural inequality operation 6", {|false !== 0|}, "true");
    (let str_max_int = string_of_int max_int in
     ("max int", str_max_int, str_max_int));
    (* (let str_min_int = string_of_int min_int in ("min int",
       str_min_int, str_min_int)); *)
    (* ("basic let defn test", {|let y = 0|}, "0"); *)
    (* ("object", {|{"x":1}|}, "<object>"); *)
    (* ("length", {|length "bigred"|}, "6"); *)
  ]

let make_interp_expr_test n in_str out_str =
  n >:: fun _ ->
  assert_equal out_str (Interp.interp_expr in_str) ~printer:Fun.id

let suite =
  "suite"
  >::: List.map (fun (n, i, o) -> make_interp_expr_test n i o) tests

let _ = run_test_tt_main suite
