use once_cell::sync::Lazy;
use std::collections::HashMap;

use crate::code::Opcode::{OpAdd, OpConstant};
use anyhow::{anyhow, Result};
use byteorder::{BigEndian, ByteOrder};
use std::convert::TryFrom;
use std::fmt::{Debug, Display, Formatter};
use std::iter::FromIterator;
use std::ops::Index;

pub struct Instructions(Vec<u8>);
impl Instructions {
    pub fn new() -> Self {
        Self(vec![])
    }
    pub fn len(&self) -> usize {
        self.0.len()
    }
    pub fn iter(&self) -> std::slice::Iter<'_, u8> {
        self.0.iter()
    }
    pub fn rest(&self, i: usize) -> &[u8] {
        &self.0[i..]
    }
}

impl Index<usize> for Instructions {
    type Output = u8;
    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
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
impl Debug for Instructions {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
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
                0 => {
                    format!("{}", def.name)
                }
                1 => {
                    format!("{} {}", def.name, operands[0])
                }
                _ => format!("ERROR: unhandled operand_count for {}\n", def.name),
            }
        }
        let mut i = 0;
        while i < self.len() {
            match lookup(self.0[i]) {
                Ok(def) => {
                    let (operands, read) = read_operands(def, self.rest(i + 1));
                    write!(f, "{:04} {}\n", i, fmt_instruction(def, &operands))?;
                    i += 1 + read;
                }
                Err(err) => return write!(f, "ERROR: {:?}\n", err),
            }
        }
        Ok(())
    }
}
#[derive(Copy, Clone)]
pub enum Opcode {
    OpConstant,
    OpAdd,
}

impl TryFrom<u8> for Opcode {
    type Error = &'static str;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(OpConstant),
            1 => Ok(OpAdd),
            _ => Err("not found opcode"),
        }
    }
}

pub struct Definition {
    name: &'static str,
    operand_widths: Vec<usize>,
}

static DEFINITIONS: Lazy<HashMap<u8, Definition>> = Lazy::new(|| {
    use Opcode::*;
    let mut map = HashMap::new();
    map.insert(
        OpConstant as u8,
        Definition {
            name: "OpConstant",
            operand_widths: vec![2],
        },
    );
    map.insert(
        OpAdd as u8,
        Definition {
            name: "OpAdd",
            operand_widths: vec![],
        },
    );
    map
});

pub fn lookup<'a>(op: u8) -> Result<&'a Definition> {
    DEFINITIONS
        .get(&op)
        .ok_or_else(|| anyhow!("opcode {} is undefined", op))
}

pub fn make(op: Opcode, operands: &[usize]) -> Instructions {
    let def = DEFINITIONS.get(&(op as u8));
    if def.is_none() {
        return Instructions(vec![]);
    }
    let def = def.unwrap();
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
            for (i, (want_byte, got_byte)) in tt.expected.iter().zip(instruction.iter()).enumerate()
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
