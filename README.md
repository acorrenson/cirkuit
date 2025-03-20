# cirkuit

A tiny tiny tiny HDL to model sequential circuits.

## Syntax

Circuits are described by one (or several) `.cirk` files.
Files are processed in a line-by-line manner.

### Comments

Lines starting with `--` are interpreted as comments, and are ignored.
For example:

```
-- my comment
```

### Block Declaration

A new named block can be declared using the command `block <name>`
where `name` is the name of the block.
Block declaration must be ended by `end`.

```
block myblock
  -- content of the block
end
```

### Inputs, Outputs, and Registers

A named block must start with declarations for its number of inputs, outputs, and registers.
For example, the following snippet declares 2 inputs, 1 output, and 1 register.

```
block myblock
  inputs 1
  inputs 2
  registers 1
  ...
end
```

### Reusing Blocks

After inputs, outputs, and registers, a block my declare a set of
other blocks it will use internally.
This is achieved by listing pairs `<name> <block-type>`.
For example, the following block uses two `and` blocks
internally:

```
block myblock
  ...

  myand1 and
  myand2 and

  ...
end
```

### Wires

To connect blocks, inputs, outputs, and registers,
blocks define a set of *wires*.
First, the keyword `wires` must be used to indicate the beginning of a the list of wires.
Then each wire is declared with the command `wire <src> <dst>`.

In a wire `wire <src> <dst>` the source `<src>` can be:
+ An **input** of the current block: `in.<n>` (where `n` is a number between `0` and the number of inputs)
+ A **register** number: `reg.<n>` (where `n` is a number between `0` and the number of registers)
+ An **input** of an internal block: `<name>.<n>` (where `name` is the name of one of the internal blocks, and `n` ranges between `0` and the number of inputs available for this type of block).

Similarly, the destination `<dst>` can be:
+ An **output** of the current block: `out.<n>` (where `n` is a number between `0` and the number of outputs)
+ A **register** number: `reg.<n>` (where `n` is a number between `0` and the number of registers)
+ An **output** of an internal block: `<name>.<n>` (where `name` is the name of one of the internal blocks, and `n` ranges between `0` and the number of outputs available for this type of block).

The following snippet is a complete example (assuming `add` is a block with 2 inputs and 2 outputs that adds two bits with carry).

```
-- a block that computes the double of its input bit
block double
  inputs 1
  outputs 2
  registers 0

  myadd add

  wires

  wire in.0 add.0
  wire in.0 add.1
  wire add.0 out.0
  wire add.1 out.1
end
```