(** Circuit Representation *)

module Gate = struct
  type t =
    | AND
    | NOT
end

module Source = struct
  type t =
    | BLOCK of { name : string; output : int }
    | REGISTER of { output : int }
    | INPUT of { id : int }
end

module Target = struct
  type t =
    | BLOCK of { name : string; input : int }
    | REGISTER of { input : int }
    | OUTPUT of { id : int }
end

module Wire = struct
  type t = {
    src : Source.t;
    dst : Target.t
  }

  let compare = compare
end

module WireSet = Set.Make(Wire)
module NameSet = Set.Make(String)

module Block = struct
  type t = {
    inputs    : int;
    outputs   : int;
    registers : int;
    blocks    : (string, string) Hashtbl.t;
    wires     : WireSet.t;
  }
  let validate (ctx : (string, t) Hashtbl.t) ({ inputs; outputs; registers; blocks; wires } : t) =
    let check_named name =
      match Hashtbl.find_opt blocks name with
      | None -> failwith @@ "undeclared block " ^ name
      | Some block_type ->
        match Hashtbl.find_opt ctx block_type with
        | None -> failwith @@ "uknown block " ^ block_type
        | Some block -> block
    in
    let check_src (src : Source.t) =
      match src with
      | Source.BLOCK { name; output } ->
        if output >= (check_named name).outputs then
          failwith (Format.sprintf "invalid wire %s.%d" name output)
      | Source.INPUT { id } ->
        if id >= inputs then
          failwith (Format.sprintf "invalid wire in.%d" id)
      | Source.REGISTER { output } ->
        if output >= registers then
          failwith (Format.sprintf "undefined register reg.%d" output)
    in
    let check_dst (src : Target.t) =
      match src with
      | Target.BLOCK { name; input } ->
        if input >= (check_named name).inputs then
          failwith (Format.sprintf "invalid wire %s.%d" name input)
      | Target.OUTPUT { id } ->
        if id >= outputs then
          failwith (Format.sprintf "invalid wire out.%d" id)
      | Target.REGISTER { input } ->
        if input >= registers then
          failwith (Format.sprintf "undefined register reg.%d" input)
    in
    let check_wire (w : Wire.t) =
      check_src w.src;
      check_dst w.dst;
      match w.src, w.dst with
      | Source.BLOCK lhs, Target.BLOCK rhs ->
        if lhs.name = rhs.name then
          failwith (Format.sprintf "cycle %s.%d <-> %s.%d" lhs.name lhs.output rhs.name rhs.input)
      | _ -> ()
    in
    WireSet.iter check_wire wires
end

module Circuit = struct
  type t = {
    blocks : (string, Block.t) Hashtbl.t;
    main   : string
  }

  let validate (c : t) =
    Hashtbl.iter (fun _ b -> Block.validate c.blocks b) c.blocks

  let to_dot fmt ?(block: string option) (c : t) =
    validate c;
    let name = Option.fold block ~some:Fun.id ~none:c.main in
    let main =
      try Hashtbl.find c.blocks name
      with _ -> failwith @@ "no block \"" ^ name ^ "\""
    in
    let print_src fmt (s : Source.t) =
      match s with
      | Source.BLOCK { name; _ } ->
        Format.fprintf fmt "%s" name
      | Source.REGISTER { output } ->
        Format.fprintf fmt "%d" (main.inputs + main.outputs + output)
      | Source.INPUT { id } ->
        Format.fprintf fmt "%d" id
    in
    let print_dst fmt (s : Target.t) =
      match s with
      | Target.BLOCK { name; _ } ->
        Format.fprintf fmt "%s" name
      | Target.REGISTER { input } ->
        Format.fprintf fmt "%d" (main.inputs + main.outputs + input)
      | Target.OUTPUT { id } ->
        Format.fprintf fmt "%d" (main.inputs + id)
    in
    let src_edge fmt (src : Source.t) =
      match src with
      | Source.BLOCK { output; _ } ->
        Format.fprintf fmt "%d" output
      | Source.REGISTER { output } ->
        Format.fprintf fmt "%d" output
      | Source.INPUT { id } ->
        Format.fprintf fmt "%d" id
    in
    let dst_edge fmt (src : Target.t) =
      match src with
      | Target.BLOCK { input; _ } ->
        Format.fprintf fmt "%d" input
      | Target.REGISTER { input } ->
        Format.fprintf fmt "%d" input
      | Target.OUTPUT { id } ->
        Format.fprintf fmt "%d" id
    in
    let print_edge fmt (w : Wire.t) =
      Format.fprintf fmt "%a - %a" src_edge w.src dst_edge w.dst
    in
    Format.fprintf fmt "digraph {\n";
    for i = 0 to main.inputs - 1 do
      Format.fprintf fmt "%d [label=\"input %d\"];\n" i i
    done;
    for i = main.inputs to main.inputs + main.outputs - 1 do
      Format.fprintf fmt "%d [label=\"output %d\"];\n" i (i - main.inputs)
    done;
    for i = main.inputs + main.outputs to main.inputs + main.outputs + main.registers - 1 do
      Format.fprintf fmt "%d [label=\"register %d\", shape=\"rectangle\"];\n" i (i - main.inputs - main.outputs)
    done;
    WireSet.iter (fun w ->
      Format.fprintf fmt "%a -> %a [label=\"%a\"];\n" print_src w.src print_dst w.dst print_edge w
    ) main.wires;
    Format.fprintf fmt "}\n"
end

let example =
  Circuit.{
    blocks = Hashtbl.of_seq (List.to_seq [
      ("main", Block.{
        inputs = 1;
        outputs = 1;
        registers = 1;
        blocks = Hashtbl.of_seq Seq.empty;
        wires  = WireSet.of_list [
          Wire.{ src = Source.INPUT { id = 0 }; dst = Target.BLOCK { name = "ADD"; input = 0 }};
          Wire.{ src = Source.INPUT { id = 0 }; dst = Target.BLOCK { name = "ADD"; input = 1 }};
          Wire.{ src = Source.BLOCK { name = "ADD"; output = 0 }; dst = Target.REGISTER { input = 0 }};
          Wire.{ src = Source.REGISTER { output = 0 }; dst = Target.OUTPUT { id = 0 }}
        ]
      })
    ]);
    main = "main"
  }
