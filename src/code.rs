use once_cell::sync::Lazy;

use anyhow::{anyhow, Result};
use byteorder::{BigEndian, ByteOrder};
use std::fmt::{Debug, Display, Formatter};
use std::iter::FromIterator;
use std::ops::{Index, IndexMut};

#[derive(Debug)]
pub struct Instructions(Vec<u8>);
impl Instructions {
    pub fn new() -> Self {
        Self(vec![])
    }
    pub fn len(&self) -> usize {
        self.0.len()
    }
    pub fn rest(&self, i: usize) -> &[u8] {
        &self.0[i..]
    }
    pub fn truncate(&mut self, len: usize) {
        self.0.truncate(len);
    }
}

impl Index<usize> for Instructions {
    type Output = u8;
    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}
impl IndexMut<usize> for Instructions {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.0[index]
    }
}
impl IntoIterator for Instructions {
    type Item = u8;
    type IntoIter = std::vec::IntoIter<Self::Item>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}
impl FromIterator<u8> for Instructions {
    fn from_iter<I: IntoIterator<Item = u8>>(iter: I) -> Self {
        let mut instruction = Instructions::new();
        for b in iter {
            instruction.0.push(b);
        }
        instruction
    }
}
impl Extend<u8> for Instructions {
    fn extend<I: IntoIterator<Item = u8>>(&mut self, iter: I) {
        for b in iter {
            self.0.push(b);
        }
    }
}

impl Display for Instructions {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        fn fmt_instruction(def: &Definition, operands: &[usize]) -> String {
            let operand_count = def.operand_widths.len();
            if operands.len() != operand_count {
                return format!(
                    "ERROR: operand len {} does not match defined {}",
                    operands.len(),
                    operand_count
                );
            }
            match operand_count {
                0 => format!("{:?}", def.opcode),
                1 => {
                    format!("{:?} {}", def.opcode, operands[0])
                }
                _ => format!("ERROR: unhandled operand_count for {:?}\n", def.opcode),
            }
        }
        let mut i = 0;
        while i < self.len() {
            match lookup(self.0[i]) {
                Ok(def) => {
                    let (operands, read) = read_operands(def, self.rest(i + 1));
                    writeln!(f, "{:04} {}", i, fmt_instruction(def, &operands))?;
                    i += 1 + read;
                }
                Err(err) => return writeln!(f, "ERROR: {:?}", err),
            }
        }
        Ok(())
    }
}
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Opcode {
    OpConstant,
    OpAdd,
    OpPop,
    OpSub,
    OpMul,
    OpDiv,
    OpTrue,
    OpFalse,
    OpEqual,
    OpNotEqual,
    OpGreaterThan,
    OpMinus,
    OpBang,
    OpJumpNotTruthy,
    OpJump,
    OpNull,
}

pub struct Definition {
    pub(crate) opcode: Opcode,
    operand_widths: Vec<usize>,
}

impl Definition {
    pub fn new(opcode: Opcode, operand_widths: Vec<usize>) -> Self {
        Self {
            opcode,
            operand_widths,
        }
    }
}

pub static DEFINITIONS: Lazy<Vec<Definition>> = Lazy::new(|| {
    use Opcode::*;
    vec![
        Definition::new(OpConstant, vec![2]),
        Definition::new(OpAdd, vec![]),
        Definition::new(OpPop, vec![]),
        Definition::new(OpSub, vec![]),
        Definition::new(OpMul, vec![]),
        Definition::new(OpDiv, vec![]),
        Definition::new(OpTrue, vec![]),
        Definition::new(OpFalse, vec![]),
        Definition::new(OpEqual, vec![]),
        Definition::new(OpNotEqual, vec![]),
        Definition::new(OpGreaterThan, vec![]),
        Definition::new(OpMinus, vec![]),
        Definition::new(OpBang, vec![]),
        Definition::new(OpJumpNotTruthy, vec![2]),
        Definition::new(OpJump, vec![2]),
        Definition::new(OpNull, vec![]),
    ]
});

pub fn lookup<'a>(op: u8) -> Result<&'a Definition> {
    DEFINITIONS
        .get(op as usize)
        .ok_or_else(|| anyhow!("opcode {} is undefined", op))
}

pub fn make(op: Opcode, operands: &[usize]) -> Instructions {
    let op = op as usize;
    if op >= DEFINITIONS.len() {
        return Instructions(vec![]);
    }
    let def = &DEFINITIONS[op];
    // opcode (1 byte) + operands (? byte)
    let instruction_len = 1 + def.operand_widths.iter().sum::<usize>();
    let mut instruction = vec![0u8; instruction_len];
    instruction[0] = op as u8;
    let mut offset = 1;
    for (i, &operand) in operands.iter().enumerate() {
        let width = def.operand_widths[i];
        match width {
            2 => {
                BigEndian::write_u16(&mut instruction[offset..], operand as u16);
            }
            _ => unreachable!(),
        }
        offset += width;
    }
    Instructions(instruction)
}

fn read_operands(def: &Definition, instructions: &[u8]) -> (Vec<usize>, usize) {
    let mut operands = vec![0; def.operand_widths.len()];
    let mut offset = 0;
    for (i, width) in def.operand_widths.iter().enumerate() {
        match width {
            2 => {
                operands[i] = read_uint16(&instructions[offset..]) as usize;
            }
            _ => unreachable!(),
        }
        offset += width;
    }
    (operands, offset)
}

pub fn read_uint16(instructions: &[u8]) -> u16 {
    BigEndian::read_u16(instructions)
}

#[cfg(test)]
mod tests {
    use crate::code::Opcode::*;
    use crate::code::{lookup, make, read_operands, Instructions, Opcode};

    #[test]
    fn test_make() {
        struct TestCase {
            op: Opcode,
            operands: Vec<usize>,
            expected: Vec<u8>,
        }
        let tests = vec![
            TestCase {
                op: OpConstant,
                operands: vec![65534],
                expected: vec![OpConstant as u8, 255, 254],
            },
            TestCase {
                op: OpAdd,
                operands: vec![],
                expected: vec![OpAdd as u8],
            },
        ];
        for tt in tests {
            let instruction = make(tt.op, &tt.operands);
            assert_eq!(
                tt.expected.len(),
                instruction.len(),
                "instruction has wrong length. want={}, got={}",
                tt.expected.len(),
                instruction.len()
            );
            for (i, (want_byte, got_byte)) in tt
                .expected
                .into_iter()
                .zip(instruction.into_iter())
                .enumerate()
            {
                assert_eq!(
                    want_byte, got_byte,
                    "wrong byte as pos {}. want={}, got={}",
                    i, want_byte, got_byte
                );
            }
        }
    }

    #[test]
    fn test_instructions_string() {
        let instructions = vec![
            make(OpAdd, &[]),
            make(OpConstant, &[2]),
            make(OpConstant, &[65535]),
        ];
        let instructions: Instructions = instructions.into_iter().flatten().collect();
        let expected = r#"0000 OpAdd
0001 OpConstant 2
0004 OpConstant 65535
"#;
        let actual = format!("{}", instructions);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_read_operands() {
        struct TestCase {
            op: Opcode,
            operands: Vec<usize>,
            bytes_read: usize,
        }
        let tests = vec![TestCase {
            op: OpConstant,
            operands: vec![65535],
            bytes_read: 2,
        }];
        for tt in tests {
            let instruction = make(tt.op, &tt.operands);
            let def =
                lookup(tt.op as u8).unwrap_or_else(|err| panic!("definition not found: {:?}", err));
            let (operands_read, n) = read_operands(def, instruction.rest(1));
            assert_eq!(tt.bytes_read, n, "n wrong.");
            for (want, got) in tt.operands.iter().zip(operands_read.iter()) {
                assert_eq!(want, got, "operand wrong.");
            }
        }
    }
}
