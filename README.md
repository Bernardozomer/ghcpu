# ghcpu

Great Haskell CPU: A simplified computer architecture simulated with Haskell

---

## Architecture

### Memory

The GHCPU has a 256 B, byte-addressable, zero-indexed address space.
Bytes 251 to 255 correspond to video memory. Any 8 bit integer stored inside it
will be displayed in decimal on the screen.

### Registers

The GHCPU has a total of seven registers. They are:

- **ACC:** Accumulator (8-bit). The ALU is able to add or subtract contents of
a memory location with the contents of the accumulator, storing the result
in the accumulator itself;
- **EQZ:** Equals Zero (1-bit). Indicates when the accumulator is equal to 0;
- **IC:** Instruction Counter (8-bit). Points to the next instruction to be
executed. Always starts at address 0;
- **IR:** Instruction Register (16-bit). Holds the value of
the next instruction to be executed;
- **MAR:** Memory Address Register (8-bit);
- **MDR:** Memory Data Register (8-bit);

### Instructions

The GHCPU has a total of nine instructions. They are:

[//]: # (Generated with https://www.tablesgenerator.com/markdown_tables)

| Code (decimal) | Code (binary)   | Instruction   | Description                                                                      |
|---------------:|:---------------:|---------------|----------------------------------------------------------------------------------|
| 2              | `00000010`      | `LOD <ad>`    | Loads contents in address `<ad>` into ACC                                        |
| 4              | `00000100`      | `STO <ad>`    | Loads address `<ad>` with contents in ACC                                        |
| 6              | `00000110`      | `JMP <ad>`    | Unconditional jump: loads IC with address `<ad>`                                 |
| 8              | `00001000`      | `JMZ <ad>`    | Conditional jump: loads IC with address `<ad>` if ACC is 0                       |
| 10             | `00001010`      | `CPE <ad>`    | Loads ACC with 0 if contents in `<ad>` are equal to those in ACC and 1 otherwise |
| 14             | `00001110`      | `ADD <ad>`    | Loads ACC with the sum of its contents with the contents in `<ad>`               |
| 16             | `00010000`      | `SUB <ad>`    | Loads ACC with the subtraction of its contents with the contents in `<ad>`       |
| 18             | `00010010`      | `NOP`         | Does not execute any operation                                                   |
| 20             | `00010100`      | `HLT`         | Halts the execution cycle                                                        |
