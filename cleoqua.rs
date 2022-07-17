use std::{
  env,
  fmt::Write as _,
  fs::File,
  io::{
    Read,
    Write as _,
  },
  path::{
    Path,
    PathBuf,
  },
  process,
};

// TODO: write more tests and examples

#[derive(Debug, Clone)]
enum TokenType {
  Int,
  Char,
  Str,

  Plus,
  Minus,
  Star,
  LessThan,
  Mod,
  EqEq,
  BangEq,

  ShiftLeft,
  ShiftRight,
  BitwiseOr,
  BitwiseAnd,

  Dup,
  Over,
  TwoOver,
  Dropp,
  Swap,

  Mem,
  Read,
  Write,
  Read64,
  Write64,
  Syscall,
  ArgV,
  ArgC,

  FPutD,
  PutD,
  PutC,

  If,
  Elif,
  While,
  Do,
  Else,
  End,

  Macro,
  MacroName,
  Load,
}

#[derive(Debug, Clone)]
struct Token {
  type_: TokenType,
  lexeme: String,

  origin: String,
  row: usize,
  col: usize,
}

macro_rules! err {
  ($origin:expr, $row:expr, $col:expr, $($msg:tt)*) => {
    eprint!("{}:{}:{}: [ERR]: ", $origin, $row + 1, $col + 1);
    eprintln!($($msg)*);
  };
  ($tok:path, $($msg:tt)*) => {
    err!($tok.origin, $tok.row, $tok.col, $($msg)*);
  };
}

macro_rules! note {
  ($origin:expr, $row:expr, $col:expr, $($msg:tt)*) => {
    eprint!("{}:{}:{}: [NOTE]: ", $origin, $row + 1, $col + 1);
    eprintln!($($msg)*);
  };
  ($tok:path, $($msg:tt)*) => {
    note!($tok.origin, $tok.row, $tok.col, $($msg)*);
  };
}

fn lex(origin: &str, s: &str) -> Vec<Token> {
  fn is_int(s: &str) -> bool {
    let mut chars = s.chars().peekable();
    let mut contain_int = false;

    while let Some('-') = chars.peek() {
      chars.next();
    }

    for ch in chars {
      if let '0'..='9' = ch {
        contain_int = true;
      } else {
        return false;
      }
    }
    contain_int
  }

  let mut tokens = Vec::new();

  for (row, line) in s.split('\n').enumerate() {
    let mut chars = line.chars().peekable();

    let mut col = 0;
    while col < line.len() {
      let mut lexeme = String::new();

      while let Some(ch) = chars.next() {
        match ch {
          ' ' | '\t' => {
            break;
          },
          '\'' if lexeme.is_empty() => {
            lexeme.push('\'');
            col += 1;

            match chars.next() {
              Some('\'') => {
                err!(origin, row, col, "Empty chars are not allowed!");
                process::exit(1);
              },
              Some(ch) => lexeme.push(ch),
              None => {
                err!(
                  origin,
                  row,
                  col,
                  "Expected a character but found end of file!"
                );
                process::exit(1);
              },
            }
            col += 1;

            match chars.next() {
              Some('\'') => lexeme.push('\''),
              Some(_) => {
                err!(
                  origin,
                  row,
                  col,
                  "Bigger than one character chars are not allowed!"
                );
                note!(
                  origin,
                  row,
                  col,
                  "Maybe you meant to use `\"` instead of `'`?"
                );
                process::exit(1);
              },
              None => {
                err!(origin, row, col, "Unclosed character literal!");
                process::exit(1);
              },
            }
          },
          '"' if lexeme.is_empty() => {
            lexeme.push('"');
            col += 1;

            while let Some(ch) = chars.peek() {
              if *ch == '"' {
                break;
              }
              lexeme.push(*ch);
              chars.next();
              col += 1;
            }

            match chars.next() {
              Some('"') => lexeme.push('"'),
              _ => {
                err!(origin, row, col, "Unclosed string delimiter!");
                note!(origin, row, col, "Multi-line strings are not supported.");
                process::exit(1);
              },
            }
          },
          '#' => while chars.next().is_some() {},
          _ => lexeme.push(ch),
        }
        col += 1;
      }

      let start = col - lexeme.len();
      let type_ = match lexeme.as_str() {
        "" => {
          col += 1;
          continue;
        },
        _ if is_int(&lexeme) => TokenType::Int,
        _ if lexeme.len() == 3 && &lexeme[0..1] == "'" && &lexeme[2..3] == "'" => TokenType::Char,
        _ if lexeme.len() > 1 && &lexeme[0..1] == "\"" && &lexeme[lexeme.len() - 1..] == "\"" => {
          TokenType::Str
        },

        "+" => TokenType::Plus,
        "-" => TokenType::Minus,
        "*" => TokenType::Star,
        "<" => TokenType::LessThan,
        "%" => TokenType::Mod,
        "==" => TokenType::EqEq,
        "!=" => TokenType::BangEq,

        "<<" => TokenType::ShiftLeft,
        ">>" => TokenType::ShiftRight,
        "|" => TokenType::BitwiseOr,
        "&" => TokenType::BitwiseAnd,

        "_" => TokenType::Dup,
        "over" => TokenType::Over,
        "2over" => TokenType::TwoOver,
        "!" => TokenType::Dropp,
        "<->" => TokenType::Swap,

        "mem" => TokenType::Mem,
        "v" => TokenType::Read,
        "^" => TokenType::Write,
        "v64" => TokenType::Read64,
        "^64" => TokenType::Write64,
        "syscall0" | "syscall1" | "syscall2" | "syscall3" | "syscall4" | "syscall5"
        | "syscall6" => TokenType::Syscall,
        "argv" => TokenType::ArgV,
        "argc" => TokenType::ArgC,

        "fputd" => TokenType::FPutD,
        "putd" => TokenType::PutD,
        "putc" => TokenType::PutC,

        "if" => TokenType::If,
        "elif" => TokenType::Elif,
        "while" => TokenType::While,
        "do" => TokenType::Do,
        "else" => TokenType::Else,
        "end" => TokenType::End,

        "macro" => TokenType::Macro,
        "load!" => TokenType::Load,
        _ if lexeme.len() > 1 && &lexeme[lexeme.len() - 1..] == "!" => TokenType::MacroName,

        _ => {
          err!(origin, row, start, "Unknown token `{lexeme}`!");
          note!(
            origin,
            row,
            start,
            "Maybe you wanted to write macro name `{lexeme}!`?"
          );
          process::exit(1);
        },
      };

      tokens.push(Token {
        type_,
        lexeme,

        origin: origin.to_string(),
        row,
        col: start,
      });

      col += 1;
    }
  }

  tokens
}

struct Macro {
  token: Token,
  body: Vec<Token>,
  expanded_count: usize,
}

fn process_macros(
  cur_file: &str,
  load_dirs: Vec<String>,
  expand_lim: usize,
  mut tokens: Vec<Token>,
) -> Vec<Token> {
  tokens.reverse();
  let mut out = Vec::new();

  let mut macros: Vec<Macro> = Vec::new();
  let mut loadeds: Vec<String> = vec![cur_file.to_string(), ["./", cur_file].concat()];

  while let Some(token) = tokens.pop() {
    match token.type_ {
      TokenType::Macro => {
        let token = match tokens.pop() {
          Some(
            token @ Token {
              type_: TokenType::MacroName,
              ..
            },
          ) => {
            if let Some(Macro {
              token: macro_token, ..
            }) = macros
              .iter()
              .find(|macro_| macro_.token.lexeme == token.lexeme)
            {
              err!(token, "Macro with name `{}` already exists!", token.lexeme);
              note!(macro_token, "`{}` was defined here.", token.lexeme);
              process::exit(1);
            }
            if token.lexeme == "load!" {
              err!(token, "Macro with name `{}` is used!", token.lexeme);
              process::exit(1);
            }
            token
          },
          Some(token) => {
            err!(token, "Macro names must end with `!`!");
            note!(token, "Maybe you wanted to write `{}!`?", token.lexeme);
            process::exit(1);
          },
          None => {
            err!(token, "Expected macro name after `macro`!");
            process::exit(1);
          },
        };
        match tokens.pop() {
          Some(Token {
            type_: TokenType::Do,
            ..
          }) => (),
          _ => {
            err!(token, "Expected `do` after macro name!");
            process::exit(1);
          },
        }

        let mut body = Vec::new();
        let mut block_depth = 0;
        while let Some(token) = tokens.pop() {
          match token.type_ {
            TokenType::End if block_depth == 0 => break,
            TokenType::End | TokenType::Elif => block_depth -= 1,
            TokenType::Do => block_depth += 1,
            _ => (),
          }
          body.push(token);
        }

        macros.push(Macro {
          token,
          body,
          expanded_count: 0,
        });
      },
      TokenType::MacroName => {
        let macro_ = match macros
          .iter_mut()
          .find(|macro_| macro_.token.lexeme == token.lexeme)
        {
          Some(macro_) if macro_.expanded_count >= expand_lim => {
            err!(
              token.origin,
              token.row,
              token.col,
              "Macro `{}` has been expanded {} times which is over the expand limit ({expand_lim})!",
              token.lexeme,
              macro_.expanded_count
            );
            process::exit(1);
          },
          Some(macro_) => macro_,
          None => {
            err!(token, "No macro with name `{}`!", token.lexeme);
            process::exit(1);
          },
        };
        macro_.expanded_count += 1;
        tokens.extend(macro_.body.clone().into_iter().rev());
      },

      TokenType::Load => {
        let file = match tokens.pop() {
          Some(Token {
            type_: TokenType::Str,
            lexeme,
            ..
          }) => lexeme[1..lexeme.len() - 1].to_string(),
          Some(token) => {
            err!(token, "`load!`'s value must be a string!");
            process::exit(1);
          },
          None => {
            err!(token, "Expected file path!");
            process::exit(1);
          },
        };

        if loadeds.iter().any(|path| path as &str == file) {
          continue;
        }

        let mut file_contents = String::new();
        let mut loaded = false;
        for load_dir in load_dirs.iter() {
          match File::open(Path::new(&load_dir).join(&file)) {
            Ok(mut f) => {
              let _ = f.read_to_string(&mut file_contents);
              loaded = true;
              break;
            },
            Err(_) => {
              continue;
            },
          }
        }

        if !loaded {
          err!(token, "Cannot open file path `{file}`!");
          process::exit(1);
        }

        let loaded_tokens = lex(&file, &file_contents);
        loadeds.push(file);
        tokens.extend(loaded_tokens.into_iter().rev());
      },

      _ => out.push(token),
    }
  }

  out
}

const MEM_LENGTH: usize = 480_000;

fn compile_to_arm64_asm(tokens: Vec<Token>) -> String {
  let mut s = String::new();

  let mut block_stack = Vec::new();
  let mut strs = Vec::new();
  let mut jmp_tracker = 0;

  let _ = writeln!(s, ".bss");
  let _ = writeln!(s, ".lcomm MEM, {MEM_LENGTH}");
  let _ = writeln!(s, ".lcomm ARG_PTR, 8");

  let _ = writeln!(s);

  let _ = writeln!(s, ".text");

  let _ = writeln!(s, "putd:");
  let _ = writeln!(s, "  stp x29, x30, [x28, -64]!");
  let _ = writeln!(s, "  mov w7, 0");
  let _ = writeln!(s, "  mov x29, x28");
  let _ = writeln!(s, "  tbz x0, #63, .L2");
  let _ = writeln!(s, "  neg x0, x0");
  let _ = writeln!(s, "  mov w7, 1");
  let _ = writeln!(s, ".L2:");
  let _ = writeln!(s, "  mov x6, -3689348814741910324");
  let _ = writeln!(s, "  add x1, x28, 16");
  let _ = writeln!(s, "  mov x2, 0");
  let _ = writeln!(s, "  movk x6, 0xcccd, lsl 0");
  let _ = writeln!(s, ".L3:");
  let _ = writeln!(s, "  umulh x4, x0, x6");
  let _ = writeln!(s, "  sub x5, x1, x2");
  let _ = writeln!(s, "  add x2, x2, 1");
  let _ = writeln!(s, "  lsr x4, x4, 3");
  let _ = writeln!(s, "  add x3, x4, x4, lsl 2");
  let _ = writeln!(s, "  sub x3, x0, x3, lsl 1");
  let _ = writeln!(s, "  mov x0, x4");
  let _ = writeln!(s, "  add w3, w3, 48");
  let _ = writeln!(s, "  strb w3, [x5, 47]");
  let _ = writeln!(s, "  cbnz x4, .L3");
  let _ = writeln!(s, "  cbz w7, .L4");
  let _ = writeln!(s, "  sub x0, x1, x2");
  let _ = writeln!(s, "  add x2, x2, 1");
  let _ = writeln!(s, "  mov w3, 45");
  let _ = writeln!(s, "  strb w3, [x0, 47]");
  let _ = writeln!(s, ".L4:");
  let _ = writeln!(s, "  sub x1, x1, x2");
  let _ = writeln!(s, "  mov x0, x20");
  let _ = writeln!(s, "  add x1, x1, 48");
  let _ = writeln!(s, "  mov x8, 64");
  let _ = writeln!(s, "  svc 0");
  let _ = writeln!(s, "  ldp x29, x30, [x28], 64");
  let _ = writeln!(s, "  ret");
  let _ = writeln!(s);

  let _ = writeln!(s, ".global _start");
  let _ = writeln!(s, "_start:");

  let _ = writeln!(s, "  mov x28, sp");
  let _ = writeln!(s, "  ldr x0, =ARG_PTR");
  let _ = writeln!(s, "  str x28, [x0]");

  for token in tokens {
    match token.type_ {
      TokenType::Int => {
        let _ = writeln!(s, "  // <-- int -->");
        let _ = writeln!(s, "  ldr x0, ={}", token.lexeme);
        let _ = writeln!(s, "  sub sp, x28, #8");
        let _ = writeln!(s, "  str x0, [x28, #-8]!");
      },
      TokenType::Char => {
        let _ = writeln!(s, "  // <-- char -->");
        let _ = writeln!(s, "  mov x0, {}", token.lexeme.as_bytes()[1]);
        let _ = writeln!(s, "  sub sp, x28, #8");
        let _ = writeln!(s, "  str x0, [x28, #-8]!");
      },
      TokenType::Str => {
        let _ = writeln!(s, "  // <-- str -->");

        let str_ = token.lexeme[1..token.lexeme.len() - 1].to_string();

        let _ = writeln!(s, "  sub sp, x28, #16");
        let _ = writeln!(s, "  ldr x0, =str_{}", strs.len());
        let _ = writeln!(s, "  str x0, [x28, #-8]!");
        let _ = writeln!(s, "  mov x0, {}", str_.as_bytes().len());
        let _ = writeln!(s, "  str x0, [x28, #-8]!");

        strs.push(str_);
      },

      TokenType::Plus => {
        let _ = writeln!(s, "  // <-- plus -->");
        let _ = writeln!(s, "  ldr x0, [x28], #8");
        let _ = writeln!(s, "  ldr x1, [x28], #8");
        let _ = writeln!(s, "  add x0, x0, x1");
        let _ = writeln!(s, "  sub sp, x28, #8");
        let _ = writeln!(s, "  str x0, [x28, #-8]!");
      },
      TokenType::Minus => {
        let _ = writeln!(s, "  // <-- minus -->");
        let _ = writeln!(s, "  ldr x1, [x28], #8");
        let _ = writeln!(s, "  ldr x0, [x28], #8");
        let _ = writeln!(s, "  sub x0, x0, x1");
        let _ = writeln!(s, "  sub sp, x28, #8");
        let _ = writeln!(s, "  str x0, [x28, #-8]!");
      },
      TokenType::Star => {
        let _ = writeln!(s, "  // <-- star -->");
        let _ = writeln!(s, "  ldr x1, [x28], #8");
        let _ = writeln!(s, "  ldr x0, [x28], #8");
        let _ = writeln!(s, "  mul x0, x0, x1");
        let _ = writeln!(s, "  sub sp, x28, #8");
        let _ = writeln!(s, "  str x0, [x28, #-8]!");
      },
      TokenType::LessThan => {
        let _ = writeln!(s, "  // <-- less than -->");
        let _ = writeln!(s, "  ldr x1, [x28], #8");
        let _ = writeln!(s, "  ldr x0, [x28], #8");
        let _ = writeln!(s, "  cmp x0, x1");
        let _ = writeln!(s, "  cset x0, lt");
        let _ = writeln!(s, "  sub sp, x28, #8");
        let _ = writeln!(s, "  str x0, [x28, #-8]!");
      },
      TokenType::Mod => {
        let _ = writeln!(s, "  // <-- mod -->");
        let _ = writeln!(s, "  ldr x1, [x28], #8");
        let _ = writeln!(s, "  ldr x0, [x28], #8");
        let _ = writeln!(s, "  udiv x2, x0, x1");
        let _ = writeln!(s, "  msub x0, x2, x1, x0");
        let _ = writeln!(s, "  sub sp, x28, #8");
        let _ = writeln!(s, "  str x0, [x28, #-8]!");
      },
      TokenType::EqEq => {
        let _ = writeln!(s, "  // <-- equal -->");
        let _ = writeln!(s, "  ldr x1, [x28], #8");
        let _ = writeln!(s, "  ldr x0, [x28], #8");
        let _ = writeln!(s, "  cmp x0, x1");
        let _ = writeln!(s, "  cset x0, eq");
        let _ = writeln!(s, "  sub sp, x28, #8");
        let _ = writeln!(s, "  str x0, [x28, #-8]!");
      },
      TokenType::BangEq => {
        let _ = writeln!(s, "  // <-- not equal -->");
        let _ = writeln!(s, "  ldr x1, [x28], #8");
        let _ = writeln!(s, "  ldr x0, [x28], #8");
        let _ = writeln!(s, "  cmp x0, x1");
        let _ = writeln!(s, "  cset x0, ne");
        let _ = writeln!(s, "  sub sp, x28, #8");
        let _ = writeln!(s, "  str x0, [x28, #-8]!");
      },

      TokenType::ShiftLeft => {
        let _ = writeln!(s, "  // <-- shift left -->");
        let _ = writeln!(s, "  ldr x1, [x28], #8");
        let _ = writeln!(s, "  ldr x0, [x28], #8");
        let _ = writeln!(s, "  lsl x0, x0, x1");
        let _ = writeln!(s, "  sub sp, x28, #8");
        let _ = writeln!(s, "  str x0, [x28, #-8]!");
      },
      TokenType::ShiftRight => {
        let _ = writeln!(s, "  // <-- shift right -->");
        let _ = writeln!(s, "  ldr x1, [x28], #8");
        let _ = writeln!(s, "  ldr x0, [x28], #8");
        let _ = writeln!(s, "  lsr x0, x0, x1");
        let _ = writeln!(s, "  sub sp, x28, #8");
        let _ = writeln!(s, "  str x0, [x28, #-8]!");
      },
      TokenType::BitwiseOr => {
        let _ = writeln!(s, "  // <-- bitwise or -->");
        let _ = writeln!(s, "  ldr x1, [x28], #8");
        let _ = writeln!(s, "  ldr x0, [x28], #8");
        let _ = writeln!(s, "  orr x0, x0, x1");
        let _ = writeln!(s, "  sub sp, x28, #8");
        let _ = writeln!(s, "  str x0, [x28, #-8]!");
      },
      TokenType::BitwiseAnd => {
        let _ = writeln!(s, "  // <-- bitwise and -->");
        let _ = writeln!(s, "  ldr x1, [x28], #8");
        let _ = writeln!(s, "  ldr x0, [x28], #8");
        let _ = writeln!(s, "  and x0, x0, x1");
        let _ = writeln!(s, "  sub sp, x28, #8");
        let _ = writeln!(s, "  str x0, [x28, #-8]!");
      },

      TokenType::Dup => {
        let _ = writeln!(s, "  // <-- dup -->");
        let _ = writeln!(s, "  ldr x0, [x28], #8");
        let _ = writeln!(s, "  sub sp, x28, #16");
        let _ = writeln!(s, "  str x0, [x28, #-8]!");
        let _ = writeln!(s, "  str x0, [x28, #-8]!");
      },
      TokenType::Over => {
        let _ = writeln!(s, "  // <-- over -->");
        let _ = writeln!(s, "  ldr x0, [x28], #8");
        let _ = writeln!(s, "  ldr x1, [x28], #8");
        let _ = writeln!(s, "  sub sp, x28, #24");
        let _ = writeln!(s, "  str x1, [x28, #-8]!");
        let _ = writeln!(s, "  str x0, [x28, #-8]!");
        let _ = writeln!(s, "  str x1, [x28, #-8]!");
      },
      TokenType::TwoOver => {
        let _ = writeln!(s, "  // <-- 2over -->");
        let _ = writeln!(s, "  ldr x0, [x28], #8");
        let _ = writeln!(s, "  ldr x1, [x28], #8");
        let _ = writeln!(s, "  ldr x2, [x28], #8");
        let _ = writeln!(s, "  sub sp, x28, #40");
        let _ = writeln!(s, "  str x2, [x28, #-8]!");
        let _ = writeln!(s, "  str x1, [x28, #-8]!");
        let _ = writeln!(s, "  str x0, [x28, #-8]!");
        let _ = writeln!(s, "  str x2, [x28, #-8]!");
        let _ = writeln!(s, "  str x1, [x28, #-8]!");
      },
      TokenType::Dropp => {
        let _ = writeln!(s, "  // <-- drop -->");
        let _ = writeln!(s, "  ldr x0, [x28], #8");
      },
      TokenType::Swap => {
        let _ = writeln!(s, "  // <-- swap -->");
        let _ = writeln!(s, "  ldr x0, [x28], #8");
        let _ = writeln!(s, "  ldr x1, [x28], #8");
        let _ = writeln!(s, "  str x0, [x28, #-8]!");
        let _ = writeln!(s, "  str x1, [x28, #-8]!");
      },

      TokenType::Mem => {
        let _ = writeln!(s, "  // <-- mem -->");
        let _ = writeln!(s, "  ldr x0, =MEM");
        let _ = writeln!(s, "  sub sp, x28, #8");
        let _ = writeln!(s, "  str x0, [x28, #-8]!");
      },
      TokenType::Read => {
        let _ = writeln!(s, "  // <-- read -->");
        let _ = writeln!(s, "  ldr x0, [x28], #8");
        let _ = writeln!(s, "  ldrb w0, [x0]");
        let _ = writeln!(s, "  sub sp, x28, #8");
        let _ = writeln!(s, "  str w0, [x28, #-8]!");
      },
      TokenType::Write => {
        let _ = writeln!(s, "  // <-- write -->");
        let _ = writeln!(s, "  ldr w0, [x28], #8");
        let _ = writeln!(s, "  ldr x1, [x28], #8");
        let _ = writeln!(s, "  strb w0, [x1]");
      },
      TokenType::Read64 => {
        let _ = writeln!(s, "  // <-- read64 -->");
        let _ = writeln!(s, "  ldr x0, [x28], #8");
        let _ = writeln!(s, "  ldr x0, [x0]");
        let _ = writeln!(s, "  sub sp, x28, #8");
        let _ = writeln!(s, "  str x0, [x28, #-8]!");
      },
      TokenType::Write64 => {
        let _ = writeln!(s, "  // <-- store64 -->");
        let _ = writeln!(s, "  ldr x0, [x28], #8");
        let _ = writeln!(s, "  ldr x1, [x28], #8");
        let _ = writeln!(s, "  str x0, [x1]");
      },
      TokenType::Syscall => {
        let _ = writeln!(s, "  // <-- syscall -->");

        let syscall_argc: usize = token.lexeme[token.lexeme.len() - 1..].parse().unwrap();

        let _ = writeln!(s, "  ldr x8, [x28], #8");
        for i in (0..syscall_argc).rev() {
          let _ = writeln!(s, "  ldr x{i}, [x28], #8");
        }

        let _ = writeln!(s, "  svc 0");
        let _ = writeln!(s, "  sub sp, x28, #8");
        let _ = writeln!(s, "  str x0, [x28, #-8]!");
      },
      TokenType::ArgV => {
        let _ = writeln!(s, "  // <-- argc -->");
        let _ = writeln!(s, "  ldr x0, =ARG_PTR");
        let _ = writeln!(s, "  ldr x0, [x0]");
        let _ = writeln!(s, "  add x0, x0, #8");
        let _ = writeln!(s, "  sub sp, x28, #8");
        let _ = writeln!(s, "  str x0, [x28, #-8]!");
      },
      TokenType::ArgC => {
        let _ = writeln!(s, "  // <-- argc -->");
        let _ = writeln!(s, "  ldr x0, =ARG_PTR");
        let _ = writeln!(s, "  ldr x0, [x0]");
        let _ = writeln!(s, "  ldr x0, [x0]");
        let _ = writeln!(s, "  sub sp, x28, #8");
        let _ = writeln!(s, "  str x0, [x28, #-8]!");
      },

      TokenType::FPutD => {
        let _ = writeln!(s, "  // <-- fputd -->");
        let _ = writeln!(s, "  ldr x0, [x28], #8");
        let _ = writeln!(s, "  ldr x20, [x28], #8");
        let _ = writeln!(s, "  bl putd");
      },
      TokenType::PutD => {
        let _ = writeln!(s, "  // <-- putd -->");
        let _ = writeln!(s, "  ldr x0, [x28], #8");
        let _ = writeln!(s, "  mov x20, #1");
        let _ = writeln!(s, "  bl putd");
      },
      TokenType::PutC => {
        let _ = writeln!(s, "  // <-- putc -->");
        let _ = writeln!(s, "  mov x8, 64");
        let _ = writeln!(s, "  mov x0, #1");
        let _ = writeln!(s, "  mov x1, x28");
        let _ = writeln!(s, "  mov x2, #1");
        let _ = writeln!(s, "  svc 0");
        let _ = writeln!(s, "  ldr x0, [x28], #8");
      },

      TokenType::If => {
        let _ = writeln!(s, "  // <-- if -->");

        jmp_tracker += 1;
        block_stack.push((token.clone(), jmp_tracker));
      },
      TokenType::Elif => {
        let _ = writeln!(s, "  // <-- elif -->");

        match block_stack.pop() {
          Some((
            Token {
              type_: TokenType::If,
              ..
            },
            jmp,
          )) => {
            let _ = writeln!(s, "  b jmp_{}", jmp_tracker + 2);
            let _ = writeln!(s, "jmp_{jmp}:");
          },
          Some((
            Token {
              type_: TokenType::Elif,
              ..
            },
            jmp,
          )) => {
            let _ = writeln!(s, "jmp_{}:", jmp + 1);
            let _ = writeln!(s, "  b jmp_{}", jmp_tracker + 2);
            let _ = writeln!(s, "jmp_{jmp}:");
          },
          Some((block_tok, _)) => {
            err!(
              token,
              "`elif` cannot be appended to `{}`!",
              block_tok.lexeme
            );
            note!(block_tok, "`{}`'s block was opened here.", block_tok.lexeme);
            process::exit(1);
          },
          None => {
            err!(token, "Found a lone `elif` block!");
            process::exit(1);
          },
        }

        jmp_tracker += 1;
        block_stack.push((token.clone(), jmp_tracker));
        jmp_tracker += 1;
      },
      TokenType::While => {
        let _ = writeln!(s, "  // <-- while -->");

        // We create a new label for `end` to jump to
        jmp_tracker += 1;
        let _ = writeln!(s, "jmp_{jmp_tracker}:");
        jmp_tracker += 1;

        block_stack.push((token.clone(), jmp_tracker));
      },
      TokenType::Do => {
        let jmp_tracker = match block_stack.last() {
          Some((_, jmp_tracker)) => jmp_tracker,
          None => {
            err!(token, "Found a lone `do`!");
            process::exit(1);
          },
        };

        let _ = writeln!(s, "  // <-- do -->");
        let _ = writeln!(s, "  ldr x0, [x28], #8");
        let _ = writeln!(s, "  cmp x0, 1");
        let _ = writeln!(s, "  b.ne jmp_{jmp_tracker}");
      },
      TokenType::Else => {
        let _ = writeln!(s, "  // <-- else -->");

        match block_stack.pop() {
          Some((
            Token {
              type_: TokenType::If,
              ..
            },
            jmp,
          )) => {
            // Jump to end if `else` was reached
            let _ = writeln!(s, "  b jmp_{}", jmp_tracker + 1);
            // Otherwise we jump to this label if `if`'s condition was falsy
            let _ = writeln!(s, "jmp_{jmp}:");
          },
          Some((
            Token {
              type_: TokenType::Elif,
              ..
            },
            jmp,
          )) => {
            // Label for elif to jump too
            let _ = writeln!(s, "jmp_{}:", jmp + 1);
            let _ = writeln!(s, "  b jmp_{}", jmp_tracker + 1);
            let _ = writeln!(s, "jmp_{jmp}:");
          },
          Some((block_tok, _)) => {
            err!(
              token,
              "`else` cannot be appended to `{}`!",
              block_tok.lexeme
            );
            note!(block_tok, "`{}`'s block was opened here.", block_tok.lexeme);
            process::exit(1);
          },
          None => {
            err!(token, "Found a lone `else` block!");
            process::exit(1);
          },
        }

        jmp_tracker += 1;
        block_stack.push((token.clone(), jmp_tracker));
      },
      TokenType::End => {
        let _ = writeln!(s, "  // <-- end -->");

        let (block_type, jmp) = match block_stack.pop() {
          Some((b, jmp_tracker)) => (b.type_, jmp_tracker),
          None => {
            err!(token, "Found a lone `end`!");
            process::exit(1);
          },
        };

        match block_type {
          // End for ifs and elses doesn't really do anything special
          TokenType::If | TokenType::Else => (),
          TokenType::Elif => {
            let _ = writeln!(s, "jmp_{}:", jmp + 1);
          },

          TokenType::While => {
            let _ = writeln!(s, "  b jmp_{}", jmp - 1);
          },

          _ => unreachable!(),
        }

        let _ = writeln!(s, "jmp_{jmp}:");
      },

      TokenType::Macro | TokenType::MacroName | TokenType::Load => unreachable!(),
    }
  }

  let _ = writeln!(s, "  // <-- exit -->");
  let _ = writeln!(s, "  mov x8, 93");
  let _ = writeln!(s, "  mov x0, 0");
  let _ = writeln!(s, "  svc 0");
  let _ = writeln!(s);

  let _ = writeln!(s, ".data");
  for (i, str_) in strs.iter().enumerate() {
    let _ = writeln!(s, "  str_{i}: .ascii \"{str_}\\0\"");
  }

  if !block_stack.is_empty() {
    for (block_tok, _) in block_stack.iter().rev() {
      err!(block_tok, "`{}` doesn't have an `end`!", block_tok.lexeme);
    }
    process::exit(1);
  }

  s
}

const DEF_EXPAND_LIM: usize = 5000;

fn usage() {
  println!("[INFO]: Usage: cleoqua [OPTIONS] <file-path>.clq");
  println!("[INFO]: OPTIONS:");
  println!("[INFO]:   --help,         -h: Prints this help message.");
  println!(
    "[INFO]:   --expand-limit, -e: Set the expand limit for macros. Default is {DEF_EXPAND_LIM}."
  );
  println!("[INFO]:   --load-dir,     -L: Adds a directory to load files from.");
}

fn main() {
  let mut file = None;
  let mut expand_lim = DEF_EXPAND_LIM;
  let mut load_dirs = vec!["./".to_string()];

  let mut args = env::args().skip(1);
  while let Some(arg) = args.next() {
    if &arg[0..1] == "-" {
      let mut arg = &arg[1..];
      if &arg[0..1] == "-" {
        arg = &arg[1..];
      }

      match arg {
        "help" | "h" => {
          usage();
          process::exit(0);
        },
        "expand-limit" | "e" => {
          let lim = match args.next() {
            Some(lim) => match lim.parse() {
              Ok(lim) => lim,
              Err(_) => {
                eprintln!("[ERR]: Expected expand limit to be an integer.");
                process::exit(1);
              },
            },
            None => {
              eprintln!("[ERR]: Expected expand limit.");
              process::exit(1);
            },
          };
          expand_lim = lim;
        },
        "load-dir" | "L" => {
          let load_dir = match args.next() {
            Some(load_dir) => load_dir,
            None => {
              eprintln!("[ERR]: Expected directory.");
              process::exit(1);
            },
          };
          load_dirs.push(load_dir);
        },
        _ => {
          usage();
          eprintln!("[ERR]: Unknown option `{arg}`");
          process::exit(1);
        },
      }
    } else if file.is_none() {
      if &arg[arg.len() - 4..] != ".clq" {
        eprintln!("[ERR]: Expected given file-path to end with `.clq`");
        process::exit(1);
      }
      file = Some(arg);
    } else {
      eprintln!("[WARN]: Unused command-line argument `{arg}`");
    }
  }

  let file = match file {
    Some(file) => file,
    None => {
      usage();
      eprintln!("[ERR]: Expected file-path");
      process::exit(1);
    },
  };

  let mut file_contents = String::new();
  let _ = match File::open(&file) {
    Ok(mut f) => f.read_to_string(&mut file_contents),
    Err(_) => {
      eprintln!("[ERR]: Cannot read file `{file}`");
      process::exit(1);
    },
  };

  let mut file_dir = PathBuf::from(&file);
  let file_name = file_dir.clone();
  let file_name = file_name.file_name().unwrap().to_string_lossy();

  file_dir.pop();
  load_dirs.push(file_dir.to_string_lossy().to_string());

  let tokens = lex(&file, &file_contents);
  let tokens = process_macros(&file_name, load_dirs, expand_lim, tokens);
  let asm = compile_to_arm64_asm(tokens);

  let mut asm_path = file[..file.len() - 4].to_string();
  asm_path.push_str(".S");
  let _ = File::create(asm_path).unwrap().write_all(asm.as_bytes());
}
