open Asm
open Csci1260
open Printf
open Yojson

type diffresult =
  { program: string
  ; expected: (string, string) result option
  ; interpreter: (string, string) result
  ; compiler: (string, string) result }

type partial_success = {interpreter_agrees: bool; compiler_agrees: bool}

let indent s =
  String.split_on_char '\n' s
  |> List.map (fun s -> "\t" ^ s)
  |> String.concat "\n"

let display_diffresult {program; expected; interpreter; compiler} : string =
  let display_outputs outputs =
    outputs
    |> List.map (fun (source, output) ->
           let descriptor, output =
             match output with
             | Ok output ->
                 ("output", output)
             | Error error ->
                 ("error", error)
           in
           sprintf "%s %s:\n\n%s" source descriptor (indent output))
    |> String.concat "\n\n"
  and expected =
    match expected with Some expected -> [("Expected", expected)] | None -> []
  and actual = [("Interpreter", interpreter); ("Compiler", compiler)] in
  sprintf "Program:\n\n%s\n\n" (indent program)
  ^ display_outputs (expected @ actual)

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
    | {program= _; expected= Some expected; interpreter; compiler} ->
        let interpreter_agrees = outputs_agree expected interpreter
        and compiler_agrees = outputs_agree expected compiler in
        ( interpreter_agrees && compiler_agrees
        , Some {interpreter_agrees; compiler_agrees} )
    | { program= _
      ; expected= None
      ; interpreter= Ok interpreter
      ; compiler= Ok compiler } ->
        (String.equal interpreter compiler, None)
    | {program= _; expected= None; interpreter= _; compiler= _} ->
        (false, None)
  in
  let summary = display_diffresult diffresult in
  if ok then Ok summary else Error (summary, partial_success)

let diff program expected =
  let ast =
    try Ok (S_exp.parse program) with e -> Error (Printexc.to_string e)
  in
  let try_run f =
    Result.bind ast (fun ast ->
        try Ok (f ast) with e -> Error (Printexc.to_string e))
  in
  let interpreter = try_run Interp.interp
  and compiler =
    try_run (fun ast ->
        let instrs = Compile.compile ast in
        Assemble.eval "test_output" Runtime.runtime "test" [] instrs)
  in
  result_of_diffresult {program; expected; interpreter; compiler}

let read_file file =
  let ch = open_in file in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch ; String.trim s

let diff_file path =
  let filename = Filename.basename path in
  let expected =
    let name = Filename.remove_extension path in
    let out_file = name ^ ".out" and err_file = name ^ ".err" in
    match (Sys.file_exists out_file, Sys.file_exists err_file) with
    | false, false ->
        None
    | false, true ->
        let reason = read_file err_file in
        let description =
          "ERROR"
          ^ if String.length reason > 0 then sprintf ": %s" reason else ""
        in
        Some (Error description)
    | true, false ->
        Some (Ok (read_file out_file))
    | true, true ->
        failwith (sprintf "Expected output and error for test: %s" filename)
  in
  diff (read_file path) expected

let csv_results =
  (try read_file "../examples/examples.csv" with _ -> "")
  |> String.split_on_char '\n'
  |> List.filter (fun line -> String.length line != 0)
  |> List.map (String.split_on_char ',')
  |> List.map (List.map String.trim)
  |> List.mapi (fun i ->
         let name = sprintf "anonymous-%s" (string_of_int i) in
         function
         | [program; expected] ->
             (name, diff program (Some (Ok expected)))
         | [program] ->
             (name, diff program None)
         | _ ->
             failwith "invalid 'examples.csv' format")

let file_results =
  Sys.readdir "../examples" |> Array.to_list
  |> List.filter (fun file -> Filename.check_suffix file ".lisp")
  |> List.map (sprintf "examples/%s")
  |> List.map (fun f -> (f, diff_file (sprintf "../%s" f)))

let results = file_results @ csv_results

let difftest () =
  printf "TESTING\n" ;
  results
  |> List.iter (function
       | name, Error (summary, _) ->
           printf "Test failed: %s\n%s\n\n" name summary
       | _, Ok _ ->
           ()) ;
  let num_tests = List.length results in
  let count f l =
    List.fold_left (fun count x -> if f x then 1 + count else count) 0 l
  in
  let failed_tests = count (fun (_, res) -> Result.is_error res) results in
  if failed_tests = 0 then printf "PASSED %d tests\n" num_tests
  else printf "FAILED %d/%d tests\n" failed_tests num_tests

let difftest_json () =
  List.map
    (fun (name, result) ->
      let result, summary, misc =
        match result with
        | Ok summary ->
            ("passed", summary, [])
        | Error (summary, partial_success) ->
            let partial_success =
              match partial_success with
              | Some {interpreter_agrees; compiler_agrees} ->
                  [ ("interpreter_agrees", `Bool interpreter_agrees)
                  ; ("compiler_agrees", `Bool compiler_agrees) ]
              | None ->
                  []
            in
            ("failed", summary, partial_success)
      in
      [ ("example", `String name)
      ; ("result", `String result)
      ; ("summary", `String summary) ]
      @ misc)
    results
  |> List.map (fun results -> `Assoc results)
  |> fun elts -> `List elts

let () =
  match Sys.getenv_opt "DIFFTEST_OUTPUT" with
  | Some "json" ->
      difftest_json () |> Yojson.to_string |> printf "%s"
  | _ ->
      difftest ()
