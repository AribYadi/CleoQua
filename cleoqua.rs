use std::io::{
  self,
  Write,
};

#[derive(Debug)]
enum TokenType {
  Int,
  Plus,
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
      if let '0'..='9' | '_' = ch {
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
          _ => lexeme.push(ch),
        }
        row += 1;
      }

      let type_ = match lexeme.as_str() {
        "" => continue,
        "+" => TokenType::Plus,
        _ if is_int(&lexeme) => TokenType::Int,
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

fn main() {
  let mut stdout = io::stdout();
  let stdin = io::stdin();

  loop {
    print!("-> ");
    let _ = stdout.flush();

    let mut buf = String::new();
    let _ = stdin.read_line(&mut buf);

    println!(" => {:?}", lex(buf.trim()));
  }
}
