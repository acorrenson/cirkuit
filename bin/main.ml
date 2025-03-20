open Cirkuit
open Lang

type task =
  | DOT


let usage_msg = "cirkuit dot -i <file.cirk> -o <file.dot>"

let input_file = ref ""

let output_file = ref ""

let block_name = ref ""

let todo : task option ref = ref None

let export_dot () =
  let oc = open_out !output_file in
  let ic = open_in !input_file in
  let circ = Parser.parse ic in
  let fmt = Format.formatter_of_out_channel oc in
  let block = !block_name in
  begin
    if block <> "" then
      Circuit.to_dot fmt ~block circ
    else
      Circuit.to_dot fmt circ
  end;
  Format.print_flush ()

let commands cmd =
  match !todo, cmd with
  | None, "dot" ->
    todo := Some DOT
  | Some _, _ ->
    raise (Arg.Bad ("only one commands can be provided (found 2)"))
  | _ ->
    raise (Arg.Bad ("invalid command \"" ^ cmd ^ "\""))

let speclist =
  [
    ("-o", Arg.Set_string output_file, "output file");
    ("-i", Arg.Set_string input_file, "output file")
  ]

let () =
  Arg.parse speclist commands usage_msg;
  match !todo with
  | None -> ()
  | Some DOT -> export_dot ()