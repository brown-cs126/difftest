open Csci1260
open Printf
open Yojson

type diffresult =
  { expected: (string, string) result option
  ; interpreter: (string, string) result
  ; compiler: (string, string) result }

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

let result_of_diffresult diffresult =
  let ok =
    match diffresult with
    | { expected= Some (Ok expected)
      ; interpreter= Ok interpreter
      ; compiler= Ok compiler } ->
        let interpreter_agrees = String.equal expected interpreter
        and compiler_agrees = String.equal expected compiler in
        interpreter_agrees && compiler_agrees
    | {expected= Some (Error _); interpreter= Error _; compiler= Error _} ->
        true
    | {expected= None; interpreter= Ok interpreter; compiler= Ok compiler} ->
        String.equal interpreter compiler
    | {expected= _; interpreter= _; compiler= _} ->
        false
  in
  let summary = print_outputs diffresult in
  if ok then Ok summary else Error summary

let diff (lang : Lang.language) example : (string, string) result =
  let read_file file =
    let ch = open_in file in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch ; s
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
  let ast = Parser.parse_file example in
  let try_run f =
    try Ok (f ast) with
    | Ast.Stuck _ as e ->
        Error (Ast.print_stuck e)
    | e ->
        Error (Printexc.to_string e)
  in
  let interpreter = try_run lang.interpreter
  and compiler =
    try_run (fun ast ->
        let filename = Filename.basename example in
        let instrs = lang.compiler ast in
        Assemble.eval "test_output" lang.runtime filename [] instrs)
  in
  result_of_diffresult {expected; interpreter; compiler}

let results lang =
  Sys.readdir "../examples" |> Array.to_list
  |> List.filter (fun file -> Filename.check_suffix file ".lisp")
  |> List.map (sprintf "examples/%s")
  |> List.map (fun f -> (f, diff lang (sprintf "../%s" f)))

let difftest (lang : Lang.language) =
  printf "TESTING %s\n" lang.name ;
  let results = results lang in
  results
  |> List.iter (fun (filename, result) ->
         match result with
         | Error summary ->
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

let difftest_json (lang : Lang.language) =
  results lang
  |> List.map (fun (example, result) ->
         let details =
           match result with
           | Ok summary ->
               [("result", `String "passed"); ("summary", `String summary)]
           | Error summary ->
               [("result", `String "failed"); ("summary", `String summary)]
         in
         `Assoc (("example", `String example) :: details))
  |> fun elts -> `List elts

let () =
  match Sys.getenv_opt "DIFFTEST_OUTPUT" with
  | Some "json" ->
      Lang.languages
      |> List.map (fun (lang : Lang.language) ->
             (lang.name, difftest_json lang))
      |> fun results -> `Assoc results |> Yojson.to_string |> printf "%s"
  | _ ->
      Lang.languages |> List.iter difftest
