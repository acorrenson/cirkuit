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
end

module Circuit = struct
  type t = {
    blocks : (string, Block.t) Hashtbl.t;
    main   : string
  }

  let to_dot fmt (c : t) =
    let main = Hashtbl.find c.blocks c.main in
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
