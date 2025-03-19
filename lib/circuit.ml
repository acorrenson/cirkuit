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
    blocks    : (string * string) array;
    wires     : WireSet.t;
  }
end

module Circuit = struct
  type t = {
    blocks : (string, Block.t) Hashtbl.t;
    main   : string
  }
end
