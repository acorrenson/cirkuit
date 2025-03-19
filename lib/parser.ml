open Circuit

let parse ic =
  let lc = ref 0 in
  let blocks = Hashtbl.create 1024 in
  let circ = Circuit.{ blocks; main = "main" } in

  let get_line_opt () =
    incr lc;
    In_channel.input_line ic
    |> Option.map String.trim
    |> Option.map (String.split_on_char ' ')
    |> Option.map (List.map String.trim)
    |> Option.map (List.filter ((<>) ""))
  in

  let get_line () =
    match get_line_opt () with
    | None -> 
      Printf.eprintf "unexpected EOF at line %d" !lc;
      exit 1
    | Some line -> line
  in

  let rec parse_circ () =
    match get_line_opt () with
    | None -> circ
    | Some [] ->
      parse_circ ()
    | Some ["block"; name] ->
      let block = parse_block () in
      Hashtbl.add blocks name block;
      parse_circ ()
    | Some ["end"] ->
      Printf.eprintf "parsing error line %d: unexpected \"end\"" !lc;
      exit 1
    | Some _ ->
      Printf.eprintf "parsing error line %d" !lc;
      exit 1

  and parse_block () =
    let inputs = parse_inputs () in
    let outputs = parse_outputs () in
    let registers = parse_registers () in
    let blocks = parse_named_blocks () |> List.to_seq |> Hashtbl.of_seq in
    let wires = parse_wires () in
    { inputs; outputs; registers; blocks; wires }

  and parse_inputs () =
    match get_line () with
    | [] -> parse_inputs ()
    | ["inputs"; n] -> int_of_string n
    | _ ->
      Printf.eprintf "inputs declaration was expected at line %d" !lc;
      exit 1

  and parse_outputs () =
    match get_line () with
    | [] -> parse_outputs ()
    | ["outputs"; n] -> int_of_string n
    | _ ->
      Printf.eprintf "outputs declaration was expected at line %d" !lc;
      exit 1

  and parse_registers () =
    match get_line () with
    | [] -> parse_registers ()
    | ["registers"; n] -> int_of_string n
    | _ ->
      Printf.eprintf "registers declaration was expected at line %d" !lc;
      exit 1

  and parse_wires () =
    match get_line () with
    | [] -> parse_wires ()
    | ["end"] -> WireSet.empty
    | ["wire"; inp; out ] ->
      let src = parse_src inp in
      let dst = parse_dst out in
      WireSet.add (Wire.{ src; dst }) (parse_wires ())
    | "wire"::_ ->
      Printf.eprintf "a wire declaration must have 2 arguments (at line %d)" !lc;
      exit 1
    | _ ->
      Printf.eprintf "wire declaration was expected at line %d" !lc;
      exit 1
 
  and parse_src str =
    match String.split_on_char '.' str with
    | ["in"; n] ->
      Source.INPUT { id = int_of_string n }
    | ["out"; _] ->
      Printf.eprintf "the source of a wire cannot be an output (at line %d)" !lc;
      exit 1
    | ["reg"; n] ->
      Source.REGISTER { output = int_of_string n }
    | [name; n] ->
      Source.BLOCK { name; output = int_of_string n }
    | _ ->
      Printf.eprintf "invalid source wire argument at line %d" !lc;
      exit 1

  and parse_dst str =
    match String.split_on_char '.' str with
    | ["out"; n] ->
      Target.OUTPUT { id = int_of_string n }
    | ["in"; _] ->
      Printf.eprintf "the destination of a wire cannot be an input (at line %d)" !lc;
      exit 1
    | ["reg"; n] ->
      Target.REGISTER { input = int_of_string n }
    | [name; n] ->
      Target.BLOCK { name; input = int_of_string n }
    | _ ->
      Printf.eprintf "invalid destination wire argument at line %d" !lc;
      exit 1
  
  and parse_named_blocks () =
    match get_line () with
    | [] -> parse_named_blocks ()
    | [name; block] ->
      (name, block)::parse_named_blocks ()
    | ["wires"] -> []
    | _ ->
      Printf.eprintf "wire declaration was expected at line %d" !lc;
    exit 1

  in
  parse_circ ()