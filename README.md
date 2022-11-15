# ghcpu

Great Haskell CPU: A simplified computer architecture simulated with Haskell

---

## Architecture

### Memory

The GHCPU has a 256 B, byte-addressable, zero-indexed address space.
Bytes 251 to 255 correspond to video memory. Any 8 bit integer stored inside it
will be displayed in decimal on the screen.

### Registers

The GHCPU has a total of four registers. They are:

| **Register** | **Name**                | **Size (bits)** | **Description**                                                                                                                                     |
|-------------:|-------------------------|-----------------|-----------------------------------------------------------------------------------------------------------------------------------------------------|
|          ACC | Accumulator             |        8        | The ALU is able to add or subtract contents of a memory location with the contents of the accumulator, storing the result in the accumulator itself |
|          EQZ | Equals Zero             |        1        | Indicates when the accumulator is equal to 0                                                                                                        |
|           IC | Instruction Counter     |        8        | Points to the next instruction to be executed. Always begins at address 0                                                                           |
|           IR | Instruction Register    |        16       | Holds the value of the next instruction to be executed                                                                                              |

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
---

## Test Programs
The GHCPU has three test program files which represent the memory with instructions and data. Each line in the test cases represent the decimal value of a byte in memory. 

The initial bytes are the program instructions, most remaining bytes are 0 which don't influence the tests. From bytes 240 to 244 are registers A,B,C,D and E which can be used to store data. Bytes 245 to 250 are numeric constants and from 251 to 255 are video memory, although only 251 (Ans) is used in these tests.

The following programs are included in the `tests` directory:

### Program 1
#### High Level Definition

```python
   Ans = A + B - 2
``` 
where A = 100, B = 75

Expected answer is 173
#### Assembly Code

```python
    0   LOD     240     # Load the value in A into ACC

    2   ADD     241     # Add the value in B to ACC
    4   SUB     245     # Subtract 2 from ACC

    6   STO     251     # Store ACC into Ans
   
    8   HLT
```
---
### Program 2
#### High Level Definition

```python
   Ans = A * B
``` 
where A = 7, B = 5

Expected answer is 35
#### Assembly Code

```python
MULT:   0   LOD     251     # Load the value in Ans into ACC
        2   ADD     240     # Add the value in A (Multiplier) to ACC 
        4   STO     251     # Store ACC into Ans
    
        6   LOD     241     # Load the value in B (Multiplicand) into ACC
        8   SUB     246     # Subtract 1 from ACC 
        10  STO     241     # Store ACC into B
    
        12  CPE     245     # Compare B to 0 
        14  JMZ     18      # Jump to END if B is equal to 0
    
        16  JMP     0       # Jump to MULT if B is different than 0
    
END:    18  HLT
```
---
### Program 3
#### High Level Definition

```python
   A = 0
   Ans = 1

   while A < 10
       A = A + 1
       Ans = Ans + 2
``` 
Expected answer is 21
#### Assembly Code

```python
        0   LOD     245     # Load 0 into ACC
        2   STO     240     # Store ACC into A

        4   LOD     246     # Load 1 into ACC
        6   STO     251     # Store ACC into Ans

LOOP:   8   LOD     240     # Load the value in A into ACC
        10  CPE     248     # Compare A to 10
        12  JMZ     28      # Jump to END if A is equal to 10
                            # If A is less then 10, continue
        14  LOD     240     # Load the value in A into ACC
        16  ADD     246     # Add 1 to ACC
        18  STO     240     # Store ACC into A

        20  LOD     251     # Load the value in Ans into ACC
        22  ADD     247     # Add 2 to ACC
        24  STO     251     # Store ACC into Ans

        26  JMP     8       # Jump to LOOP

END:    28  HLT
```