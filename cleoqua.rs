use std::{
  fs::File,
  io::{
    Read,
    Write,
  },
};

#[derive(Debug)]
enum TokenType {
  Int,
  Char,
  Plus,
  PutD,
  PutC,
}

#[derive(Debug)]
struct Token {
  type_: TokenType,
  lexeme: String,

  row: usize,
  col: usize,
}

fn lex(s: &str) -> Vec<Token> {
  fn is_int(s: &str) -> bool {
    for ch in s.chars() {
      if let '0'..='9' = ch {
      } else {
        return false;
      }
    }
    true
  }

  let mut tokens = Vec::new();

  for (col, line) in s.split('\n').enumerate() {
    let mut chars = line.chars();

    let mut row = 0;
    while row < line.len() {
      let mut lexeme = String::new();

      while let Some(ch) = chars.next() {
        match ch {
          ' ' => break,
          '\'' if lexeme == "" => {
            lexeme.push('\'');
            row += 1;

            match chars.next() {
              Some('\'') => todo!("Report an error."),
              Some(ch) => lexeme.push(ch),
              None => todo!("Report an error."),
            }
            row += 1;

            match chars.next() {
              Some('\'') => lexeme.push('\''),
              _ => todo!("Report an error."),
            }
            row += 1;

            break;
          },
          _ => lexeme.push(ch),
        }
        row += 1;
      }

      let type_ = match lexeme.as_str() {
        "" => continue,
        "+" => TokenType::Plus,
        "putd" => TokenType::PutD,
        "putc" => TokenType::PutC,
        _ if is_int(&lexeme) => TokenType::Int,
        _ if lexeme.len() == 3 && &lexeme[0..1] == "'" && &lexeme[2..3] == "'" => TokenType::Char,
        _ => todo!("Report an error."),
      };

      let start = row - lexeme.len();
      tokens.push(Token {
        type_,
        lexeme,

        row: start,
        col,
      });

      row += 1;
    }
  }

  tokens
}

fn compile_to_arm64_asm(tokens: Vec<Token>) -> String {
  let mut s = String::new();

  s.push_str(".text\n");

  s.push_str("putd:\n");
  s.push_str("  stp x29, x30, [x28, -48]!\n");
  s.push_str("  mov x7, -3689348814741910324\n");
  s.push_str("  mov x2, 0\n");
  s.push_str("  add x1, x28, 16\n");
  s.push_str("  movk x7, 0xcccd, lsl 0\n");
  s.push_str("  mov x29, x28\n");
  s.push_str(".L2:\n");
  s.push_str("  umulh x4, x0, x7\n");
  s.push_str("  sub x5, x1, x2\n");
  s.push_str("  mov x6, x0\n");
  s.push_str("  add x2, x2, 1\n");
  s.push_str("  lsr x4, x4, 3\n");
  s.push_str("  add x3, x4, x4, lsl 2\n");
  s.push_str("  sub x3, x0, x3, lsl 1\n");
  s.push_str("  mov x0, x4\n");
  s.push_str("  add w3, w3, 48\n");
  s.push_str("  strb w3, [x5, 31]\n");
  s.push_str("  cmp x6, 9\n");
  s.push_str("  bhi .L2\n");
  s.push_str("  sub x1, x1, x2\n");
  s.push_str("  mov w0, 1\n");
  s.push_str("  add x1, x1, 32\n");
  s.push_str("  mov x8, 0x40\n");
  s.push_str("  svc 0\n");
  s.push_str("  ldp x29, x30, [x28], 48\n");
  s.push_str("  ret\n");
  s.push_str("\n");

  s.push_str(".global _start\n");
  s.push_str("_start:\n");

  s.push_str("  mov x28, sp\n");

  for token in tokens {
    match token.type_ {
      TokenType::Int => {
        s.push_str("  // <-- int -->\n");
        s.push_str(&format!("  mov x0, {}\n", token.lexeme));
        s.push_str("  sub sp, x28, #8\n");
        s.push_str("  str x0, [x28, #-8]!\n");
      },
      TokenType::Char => {
        s.push_str("  // <-- char -->\n");
        s.push_str(&format!("  mov x0, {}\n", token.lexeme.as_bytes()[1]));
        s.push_str("  sub sp, x28, #8\n");
        s.push_str("  str x0, [x28, #-8]!\n");
      },
      TokenType::Plus => {
        s.push_str("  // <-- plus -->\n");
        s.push_str("  ldr x0, [x28], #8\n");
        s.push_str("  ldr x1, [x28], #8\n");
        s.push_str("  add x0, x0, x1\n");
        s.push_str("  sub sp, x28, #8\n");
        s.push_str("  str x0, [x28, #-8]!\n");
      },
      TokenType::PutD => {
        s.push_str("  // <-- putd -->\n");
        s.push_str("  ldr x0, [x28], #8\n");
        s.push_str("  bl putd\n");
      },
      TokenType::PutC => {
        s.push_str("  // <-- putc -->\n");
        s.push_str("  mov x8, 0x40\n");
        s.push_str("  mov x0, #1\n");
        s.push_str("  mov x1, x28\n");
        s.push_str("  mov x2, #1\n");
        s.push_str("  svc 0\n");
      },
    }
  }

  s.push_str("  // <-- putd -->\n");
  s.push_str("  mov x8, 0x5D\n");
  s.push_str("  mov x0, 0\n");
  s.push_str("  svc 0\n");

  s
}

fn main() {
  let mut file_contents = String::new();
  let _ = File::open("./foo.clq")
    .unwrap()
    .read_to_string(&mut file_contents);

  let tokens = lex(&file_contents);
  let asm = compile_to_arm64_asm(tokens);

  let _ = File::create("./foo.S").unwrap().write_all(asm.as_bytes());
}
