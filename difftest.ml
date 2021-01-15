open Asm
open Csci1260
open Printf
open Yojson

type diffresult =
  { expected: (string, string) result option
  ; interpreter: (string, string) result
  ; compiler: (string, string) result }

type partial_success = {interpreter_agrees: bool; compiler_agrees: bool}

let print_outputs {expected; interpreter; compiler} : string =
  let print_outputs (outputs : (string * string) list) : string =
    outputs
    |> List.map (fun (source, output) ->
           sprintf "%s output:\n\n" source ^ sprintf "\t%s\n" output)
    |> String.concat "\n"
  and expected =
    match expected with
    | Some (Ok output) ->
        [("Expected", output)]
    | Some (Error reason) ->
        [ ( "Expected"
          , "ERROR"
            ^ if String.length reason > 0 then sprintf ": %s" reason else "" )
        ]
    | None ->
        []
  and interpreter =
    ( "Interpreter"
    , match interpreter with Ok output -> output | Error err -> err )
  and compiler =
    ("Compiler", match compiler with Ok output -> output | Error err -> err)
  in
  print_outputs (expected @ [interpreter] @ [compiler])

let outputs_agree expected actual =
  match (expected, actual) with
  | Ok expected, Ok actual ->
      String.equal expected actual
  | Error _, Error _ ->
      true
  | Ok _, Error _ | Error _, Ok _ ->
      false

let result_of_diffresult diffresult =
  let ok, partial_success =
    match diffresult with
    | {expected= Some expected; interpreter; compiler} ->
        let interpreter_agrees = outputs_agree expected interpreter
        and compiler_agrees = outputs_agree expected compiler in
        ( interpreter_agrees && compiler_agrees
        , Some {interpreter_agrees; compiler_agrees} )
    | {expected= None; interpreter= Ok interpreter; compiler= Ok compiler} ->
        (String.equal interpreter compiler, None)
    | {expected= None; interpreter= _; compiler= _} ->
        (false, None)
  in
  let summary = print_outputs diffresult in
  if ok then Ok summary else Error (summary, partial_success)

let diff example : (string, string * partial_success option) result =
  let read_file file =
    let ch = open_in file in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch ;
    let n = String.length s in
    (* trim trailing newline if present *)
    if n > 0 && s.[n - 1] = '\n' then String.sub s 0 (n - 1) else s
  in
  let expected =
    let example = Filename.remove_extension example in
    let out_file = example ^ ".out" and err_file = example ^ ".err" in
    match (Sys.file_exists out_file, Sys.file_exists err_file) with
    | false, false ->
        None
    | false, true ->
        Some (Error (read_file err_file))
    | true, false ->
        Some (Ok (read_file out_file))
    | true, true ->
        failwith (sprintf "Expected output and error for test: %s" example)
  in
  let ast = S_exp.parse_file example in
  let try_run f = try Ok (f ast) with e -> Error (Printexc.to_string e) in
  let interpreter = try_run Interp.interp
  and compiler =
    try_run (fun ast ->
        let filename = Filename.basename example in
        let instrs = Compile.compile ast in
        Assemble.eval "test_output" Runtime.runtime filename [] instrs)
  in
  result_of_diffresult {expected; interpreter; compiler}

let results =
  Sys.readdir "../examples" |> Array.to_list
  |> List.filter (fun file -> Filename.check_suffix file ".lisp")
  |> List.map (sprintf "examples/%s")
  |> List.map (fun f -> (f, diff (sprintf "../%s" f)))

let difftest () =
  printf "TESTING\n" ;
  results
  |> List.iter (fun (filename, result) ->
         match result with
         | Error (summary, _) ->
             printf "Test failed: %s\n%s\n\n" filename summary
         | Ok _ ->
             ()) ;
  let num_tests = List.length results in
  let count f l =
    List.fold_left (fun count x -> if f x then 1 + count else count) 0 l
  in
  let failed_tests = count (fun (_, res) -> Result.is_error res) results in
  if failed_tests = 0 then printf "PASSED %d tests\n" num_tests
  else printf "FAILED %d/%d tests\n" failed_tests num_tests

let difftest_json () =
  results
  |> List.map (fun (example, result) ->
         let details =
           match result with
           | Ok summary ->
               [("result", `String "passed"); ("summary", `String summary)]
           | Error (summary, partial_success) ->
               let partial_success =
                 match partial_success with
                 | None ->
                     []
                 | Some {interpreter_agrees; compiler_agrees} ->
                     [ ("interpreter_agrees", `Bool interpreter_agrees)
                     ; ("compiler_agrees", `Bool compiler_agrees) ]
               in
               [("result", `String "failed"); ("summary", `String summary)]
               @ partial_success
         in
         `Assoc (("example", `String example) :: details))
  |> fun elts -> `List elts

let () =
  match Sys.getenv_opt "DIFFTEST_OUTPUT" with
  | Some "json" ->
      difftest_json () |> Yojson.to_string |> printf "%s"
  | _ ->
      difftest ()
