# asm584

asm584 translates programs written in K584VM1 assembly language into binary `.x584` files. The resulting files can be opened in [X584 simulator](https://github.com/kodemeister/X584) for further editing and debugging.

## Quick Start

Download the latest release of asm584 from the [Releases](https://github.com/kodemeister/asm584/releases) page and unpack it to any folder.

Create a text file named `max.asm` in the same folder where the unpacked executable file `asm584.exe` is located. Paste the following code into the file:

```
/*
 * Author: John Doe
 * Description: Calculate the maximum of two 16-bit unsigned integers.
 */

    RF0 := DI                              ; Input A
    RF1 := DI                              ; Input B

    WR := RF0 + ALUCIN (ALUCIN=0)          ; Copy A to WR
    WR := WR - RF1 - 1 + ALUCIN (ALUCIN=1) ; Calculate A - B
    WR := ASL(WR + ALUCIN) (ALUCIN=0)      ; Check the sign bit
    if !WRLFT then out_a else out_b

out_a:
    DO := RF0                              ; Output A
    goto end

out_b:
    DO := RF1                              ; Output B
    goto end

end:
    NOP
```

Then open a terminal, navigate to the folder where you unpacked the `asm584.exe` executable and saved the `max.asm` file, and run the assembler as shown below:

```bat
cd C:\path\to\asm584-x.x.x.x
.\asm584.exe max.asm
```

This should generate an output file named `max.x584` in the current folder. Now you can use [X584 simulator](https://github.com/kodemeister/X584) to open `max.x584` and run the assembled program.

## Running asm584

To assemble a source file from the command line, run the following command in the folder where `asm584.exe` is located:

```bat
.\asm584.exe <filename>
```

For example:

```bat
.\asm584.exe program.asm
```

asm584 will take the input file `program.asm` and produce an output file with the same name but with the `.x584` extension, i.e., `program.x584`.

If you want to specify a custom output file name, you can use the `-o` or `--output` option followed by the desired file name or path:

```bat
.\asm584.exe program.asm -o myfolder\myfile.x584
```

If you want to check which version of asm584 you are using, you can use the `-v` or `--version` option:

```bat
.\asm584.exe --version
```

To get further usage instructions, try running asm584 with the `-h` or `--help` option:

```bat
.\asm584.exe --help
```

## K584VM1 Assembly Language

A program written in K584VM1 assembly language consists of a series of statements. Every statement contains a mandatory microinstruction and several optional fields shown below in square brackets:

```
[label:] [break] microinstruction [control statement] [; X584 comment]
```

asm584 places no restrictions on white spaces within a statement. You can indent your code with any number of spaces, add spaces between fields, or move any fields to the next line for better readability.

The assembly language is case insensitive. This means that keywords, registers, labels and other elements of the language can be written in any combination of upper and lower case. For instance, `ALUCIN`, `Alucin`, and `alucin` are considered the same keyword.

### Microinstructions

asm584 supports the full microinstruction set of K584VM1 presented in the following table:

| Opcode        | Mnemonic                                      |
| ------------- | --------------------------------------------- |
| `OP   00 RF ` | `RF := RF op WR`                              |
| `OP   01 RF ` | `WR := RF op WR`                              |
| `OP   11 000` | `DO := DI op WR`                              |
| `OP   11 001` | `WR := DI op WR`                              |
| `OP   11 011` | `WR := DI op XWR`                             |
| `OP   11 100` | `XWR := DI op WR`                             |
| `OP   11 110` | `XWR := DI op XWR`                            |
| `OP   11 111` | `DO := DI op XWR`                             |
| `0011 10 RF ` | `XWR := RF + WR + ALUCIN`                     |
| `0100 10 RF ` | `WR := RF + DI + ALUCIN`                      |
| `0101 10 RF ` | `XWR := RF + DI + ALUCIN`                     |
| `0111 10 RF ` | `RF := RF + DI + ALUCIN`                      |
| `1100 10 RF ` | `WR := RF + XWR + ALUCIN`                     |
| `1101 10 RF ` | `XWR := RF + XWR + ALUCIN`                    |
| `1110 10 RF ` | `RF := XWR + ALUCIN`                          |
| `0011 11 010` | `XWR := WR + DI + ALUCIN`                     |
| `0111 11 010` | `DO := WR + DI + ALUCIN`                      |
| `1100 11 010` | `WR := XWR + DI + ALUCIN`                     |
| `1101 11 010` | `XWR := XWR + DI + ALUCIN`                    |
| `1110 11 010` | `DO := XWR + ALUCIN`                          |
| `1111 10 RF ` | `RF := DI`                                    |
| `0000 10 RF ` | `DO := RF`                                    |
| `0001 10 RF ` | `XWR := RF`                                   |
| `0110 10 000` | `WR := DI`                                    |
| `0001 11 010` | `XWR := DI`                                   |
| `1111 11 010` | `DO := DI`                                    |
| `1000 11 010` | `(WR, XWR) := RSL(WR - DI - 1 + ALUCIN, XWR)` |
| `1001 11 010` | `(WR, XWR) := RSL(WR + DI + ALUCIN, XWR)`     |
| `1000 10 RF ` | `(WR, XWR) := RSL(WR - RF - 1 + ALUCIN, XWR)` |
| `1001 10 RF ` | `(WR, XWR) := RSL(WR + RF + ALUCIN, XWR)`     |
| `1010 10 000` | `(WR, XWR) := ASR(WR + ALUCIN, XWR)`          |
| `0010 11 010` | `(WR, XWR) := ASR(WR - DI - 1 + ALUCIN, XWR)` |
| `1011 11 010` | `(WR, XWR) := ASR(WR + DI + ALUCIN, XWR)`     |
| `0010 10 RF ` | `(WR, XWR) := ASR(WR - RF - 1 + ALUCIN, XWR)` |
| `1011 10 RF ` | `(WR, XWR) := ASR(WR + RF + ALUCIN, XWR)`     |
| `0000 11 101` | `WR := ASR(WR + ALUCIN)`                      |
| `0001 11 101` | `WR := RSR(WR + ALUCIN)`                      |
| `0010 11 101` | `WR := ASL(WR + ALUCIN)`                      |
| `0011 11 101` | `WR := RSL(WR + ALUCIN)`                      |
| `1000 11 101` | `WR := LSR(WR + ALUCIN)`                      |
| `1010 11 101` | `WR := LSL(WR + ALUCIN)`                      |
| `0101 11 101` | `(WR, XWR) := RSR(WR + ALUCIN, XWR)`          |
| `0110 11 101` | `(WR, XWR) := ASL(WR + ALUCIN, XWR)`          |
| `0111 11 101` | `(WR, XWR) := RSL(WR + ALUCIN, XWR)`          |
| `1100 11 101` | `(WR, XWR) := LSR(WR + ALUCIN, XWR)`          |
| `1110 11 101` | `(WR, XWR) := LSL(WR + ALUCIN, XWR)`          |

asm584 also provides a special microinstruction `NOP` with an undocumented opcode `0100 11 010`. This microinstruction does nothing in X584.

The following ALU operations are available for microinstructions #1-#8:

| OP Field | Operation            |
| -------- | -------------------- |
| `0000`   | `!ALUCIN`            |
| `0001`   | `B - A - 1 + ALUCIN` |
| `0010`   | `A - B - 1 + ALUCIN` |
| `0011`   | `A + B + ALUCIN`     |
| `0100`   | `B + ALUCIN`         |
| `0101`   | `!B + ALUCIN`        |
| `0110`   | `A + ALUCIN`         |
| `0111`   | `!A + ALUCIN`        |
| `1000`   | `A and B`            |
| `1001`   | `A xor B`            |
| `1010`   | `!(A xor B)`         |
| `1011`   | `!A and B`           |
| `1100`   | `A and !B`           |
| `1101`   | `A or !B`            |
| `1110`   | `!A or B`            |
| `1111`   | `A or B`             |

If a microinstruction checks the state of `ALUCIN`, you should specify either `(ALUCIN=0)` or `(ALUCIN=1)` right after the mnemonic. Otherwise, you will get a syntax error about the missing value of `ALUCIN`.

Below are some examples that illustrate the syntax of K584VM1 microinstructions:

```
    RF0 := DI

    // You can also use "=" instead of ":="
    RF1 = DI

    // Here you need to specify "(ALUCIN=0)"
    WR := RF0 + ALUCIN (ALUCIN=0)

    // Alternatively, you can write "C" instead of "ALUCIN" for brevity
    WR := WR - RF1 - 1 + C (C=1)

    XWR := DI and WR
```

You can arrange the operands of arithmetic and logical expressions in any order. For example, the following microinstructions are equivalent:

```
    DO := DI - WR - 1 + ALUCIN (ALUCIN=0)
    DO := DI - WR + ALUCIN - 1 (ALUCIN=0)
    DO := DI - 1 + ALUCIN - WR (ALUCIN=0)
    DO := -WR + DI - 1 + ALUCIN (ALUCIN=0)
```

However, when you open the assembled `.x584` file in X584 simulator, the operands will be displayed in their canonical order shown in the microinstruction table above.

You can also omit `ALUCIN` from the microinstruction mnemonic by replacing it with the literal values 0 or 1, and optionally simplify the resulting expression. In this case you do not need to specify `(ALUCIN=0)` or `(ALUCIN=1)` after the mnemonic. The table below presents several examples of microinstructions where `ALUCIN` is either explicitly specified or omitted:

| `ALUCIN` Explicitly Specified           | `ALUCIN` Omitted                                 |
| --------------------------------------- | ------------------------------------------------ |
| `DO := !ALUCIN (ALUCIN=0)`              | `DO := !0`                                       |
| `DO := !ALUCIN (ALUCIN=1)`              | `DO := !1`,<br/>`DO := 0`                        |
| `DO := WR + ALUCIN (ALUCIN=0)`          | `DO := WR + 0`,<br/>`DO := WR`                   |
| `DO := WR + ALUCIN (ALUCIN=1)`          | `DO := WR + 1`                                   |
| `DO := DI - WR - 1 + ALUCIN (ALUCIN=0)` | `DO := DI - WR - 1 + 0`,<br/>`DO := DI - WR - 1` |
| `DO := DI - WR - 1 + ALUCIN (ALUCIN=1)` | `DO := DI - WR - 1 + 1`,<br/>`DO := DI - WR`     |

X584 always displays microinstructions in their canonical form, with `ALUCIN` explicitly specified.

### Labels

An optional label can be placed at the beginning of a statement. A label must start with a letter or underscore followed by letters, digits or underscores, and ended with a colon. During assembly, asm584 assigns every label a numeric address of the current microinstruction. Then you can use labels instead of addresses in `if` or `goto` control statements like the following:

```
begin:
    WR := WR + ALUCIN (ALUCIN=1)
    if ALUCOUT then end
    DO := WR + ALUCIN (ALUCIN=0)
    goto begin

end:
    NOP
```

### Breakpoints

You can prefix any microinstruction with a `break` keyword. This allows you to set breakpoints directly from the assembly code rather than clicking on the gutter in X584. For example:

```
    break DO := RF0
```

### Control Statements

Control statements are a special feature of X584 simulator. They allow to analyze certain outputs of K584VM1 microprocessor, change the program counter and so on. K584VM1 itself does not support control statements. They are supposed to be executed by external hardware, such as K584VU1.

Each microinstruction can have only one optional control statement. It should be placed after either the microinstruction or X584 comment.

asm584 supports the following control statements:

1. **Conditional statement** in the form `if <condition> then <label or address> [else <label or address>]`.

   A condition is basically a name of one of K584VM1 outputs listed in the table below. Most of outputs have a few alternative names. You can pick any of them for brevity.

   | Output     | Alternative Names |
   | ---------- | ----------------- |
   | `ALUCOUT`  | `CO3`, `C`        |
   | `ALUCOUT2` | `CO2`, `C2`       |
   | `ALUCOUT1` | `CO1`, `C1`       |
   | `ALUCOUT0` | `CO0`, `C0`       |
   | `!WRRT`    | `!SR1`            |
   | `!WRLFT`   | `!SL1`            |
   | `!XWRRT`   | `!SR2`            |
   | `!XWRLFT`  | `!SL2`            |
   | `XWR0`     |                   |
   | `XWR3`     |                   |
   | `AMSB`     | `A15`             |
   | `BMSB`     | `B15`             |

   Examples:

   ```
       WR := !RF0 + ALUCIN (ALUCIN=1) if ALUCOUT then zero else nonzero
       WR := !RF1 + ALUCIN (ALUCIN=1) if C then zero

       // You can also use numeric addresses although it is not recommended
       WR := !RF0 + ALUCIN (ALUCIN=1) if ALUCOUT then 4 else 5

       // It is possible to move the control statement to the next line for readability
       WR := ASL(WR + ALUCIN) (ALUCIN=0)
       if !WRLFT then zero

   zero:
       DO := !ALUCIN (ALUCIN=1)
   nonzero:
       DO := !ALUCIN (ALUCIN=0)
   ```

2. **Goto statement** in the form `goto <label or address>`.

   Examples:

   ```
       DO := RF0 goto end

       // You can also use numeric addresses although it is not recommended
       DO := RF0 goto 3

       // It is possible to move control statements to the next line for readability
       DO := RF0
       goto end

   end:
       NOP
   ```

3. **Input statement** in the form `input <number>`.

   If you do not want to enter the value of DI every time you run the program, you can specify it directly in the assembly code using the `input` control statement. It accepts a 16-bit number in one of the following formats:

   - A 16-digit binary number.
   - A binary number divided into 4 groups of 4 digits separated by space.
   - A hexadecimal number in the range [0x0000, 0xFFFF].
   - A decimal number in the range [-32768, 65535].

   Examples:

   ```
       RF0 := DI input 0000000100100011
       RF1 := DI input 0000 0001 0010 0011
       RF2 := DI input 0x1234
       RF3 := DI input 4660
       RF4 := DI input -1234
   ```

### Comments

asm584 supports three types of comments:

1. **Multi-line comment** that starts with `/*` and ends with `*/`. Any text between `/*` and `*/` is ignored by the assembler and not saved to `.x584` file. You can place multi-line comments anywhere in the source code. Use them for large blocks of text, such as file headers or algorithm descriptions:

   ```
   /*
    * Author: John Doe
    * Description: Calculate the maximum of two 16-bit unsigned integers.
    */
   ```

2. **Single-line comment** that starts with `//`. Any text between `//` and the end of the line is ignored by the assembler and not saved to `.x584` file. Like multi-line comments, you can place single-line comments anywhere in the source code. Use them to explain or document sections of code, for example:

   ```
       // Generate a bitmask 1000...000 for the sign bit
       WR := !ALUCIN (ALUCIN=1)
       WR := WR + ALUCIN (ALUCIN=1)
       WR := RSR(WR + ALUCIN) (ALUCIN=0)
   ```

3. **X584 comment** that starts with `;` or `#`. Any text between `;` or `#` and the end of the line will be saved to `.x584` file and displayed under the "Comment" column in X584. Each microinstruction can have only one optional X584 comment. It should be placed after either the microinstruction or the control statement. Use X584 comments to explain the meaning of single microinstructions that you want to appear in X584, for example:

   ```
       RF0 := DI ; Input A
       RF1 := DI ; Input B

       WR := !RF0 + ALUCIN (ALUCIN=1) if ALUCOUT then zero # Check whether A is zero

       WR := !RF1 + ALUCIN (ALUCIN=1) # Check whether B is zero
       if ALUCOUT then zero

   zero:
       DO := !ALUCIN (ALUCIN=1)
   ```
