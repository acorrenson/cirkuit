open Lang

let error lc s =
  Printf.eprintf "error at line %d: %s\n" lc s;
  exit 1

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

  let rec get_real_line_opt () =
    match get_line_opt () with
    | Some [] -> get_real_line_opt ()
    | Some ("--"::_) -> get_real_line_opt ()
    | ln -> ln
  in

  let get_line () =
    match get_real_line_opt () with
    | None -> 
      error !lc "unexpected EOF"
    | Some line -> line
  in

  let rec parse_circ () =
    match get_real_line_opt () with
    | None -> circ
    | Some [] ->
      parse_circ ()
    | Some ["block"; name] ->
      let block = parse_block () in
      Hashtbl.add blocks name block;
      parse_circ ()
    | Some ["end"] ->
      error !lc "unexpected \"end\""
    | Some (tok::_) ->
      error !lc @@ "unexpected tocken \"" ^ tok ^ "\""

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
    | tok::_ ->
      error !lc @@ "inputs declaration expected, found \""  ^ tok ^ "\""

  and parse_outputs () =
    match get_line () with
    | [] -> parse_outputs ()
    | ["outputs"; n] -> int_of_string n
    | tok::_ ->
      error !lc @@ "outputs declaration expected, found \"" ^ tok ^ "\""

  and parse_registers () =
    match get_line () with
    | [] -> parse_registers ()
    | ["registers"; n] -> int_of_string n
    | tok::_ ->
      error !lc @@ "registers declaration expected, found \""  ^ tok ^ "\""

  and parse_wires () =
    match get_line () with
    | [] -> parse_wires ()
    | ["end"] -> WireSet.empty
    | ["wire"; inp; out ] ->
      let src = parse_src inp in
      let dst = parse_dst out in
      WireSet.add (Wire.{ src; dst }) (parse_wires ())
    | "wire"::args ->
      error !lc
        @@ Printf.sprintf "a wire declaration must have 2 arguments (found %d)"
        @@ List.length args
    | tok::_ ->
      error !lc @@ "wire declaration expected, found \"" ^ tok ^ "\""
 
  and parse_src str =
    match String.split_on_char '.' str with
    | ["in"; n] ->
      Source.INPUT { id = int_of_string n }
    | ["out"; _] ->
      error !lc "the source of a wire cannot be an output"
    | ["reg"; n] ->
      Source.REGISTER { output = int_of_string n }
    | [name; n] ->
      Source.BLOCK { name; output = int_of_string n }
    | _ ->
      error !lc "invalid source wire argument"

  and parse_dst str =
    match String.split_on_char '.' str with
    | ["out"; n] ->
      Target.OUTPUT { id = int_of_string n }
    | ["in"; _] ->
      error !lc "the destination of a wire cannot be an input";
    | ["reg"; n] ->
      Target.REGISTER { input = int_of_string n }
    | [name; n] ->
      Target.BLOCK { name; input = int_of_string n }
    | _ ->
      error !lc "invalid destination wire argument at line";
  
  and parse_named_blocks () =
    match get_line () with
    | [] -> parse_named_blocks ()
    | [name; block] ->
      (name, block)::parse_named_blocks ()
    | ["wires"] -> []
    | tok::_ ->
      error !lc @@ "wires declaration was expected, found \"" ^ tok ^ "\""
  
  in parse_circ ()