use std::char;

fn main() {
    let mut s = String::new();
    let ss = &mut s;
    let mut ts = vec![];
        let l = Lexer::new(
            r#"
#include <stdio.h>

int main(void) {
    printf("Hello, World!");
    return 0;
}
            "#,
            ss,
        );
        let mut lit = l.into_iter();
        while let Some(Ok(tok)) = lit.next() {
            ts.push(tok);
        }

    for tok in ts {
        match tok {
            LexToken::Punct(p) => println!("Punct({:?})", p as char),
            LexToken::Ident(tok) | LexToken::DoubleQuStr(tok) => {
                println!("({:#?})", tok.upgrade(&s).unwrap())
            }
            t => println!("{t:?}"),
        }
    }
}

type Result<T, E = LexError> = core::result::Result<T, E>;

impl<'a, 's> Iterator for Lexer<'a, 's> {
    type Item = Result<LexToken<StillMutatingStore>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.input_stream.len() == 0 || self.cur_stream.len() == 0 {
            return None;
        }
        let mut its = self.cur_stream[self.cur_skip_bytes..]
            .into_iter()
            .enumerate();
        loop {
            match its.next()? {
                (_, &ch) if ch.is_ascii_whitespace() => self.cur_skip_bytes += 1,
                (_, &ch) if (ch as char == '/' && self.expect_ch('/')) || ch as char == '#' => {
                    self.cur_skip_bytes += 1;
                    'comment_or: loop {
                        self.cur_skip_bytes += 1;
                        match its.next() {
                            Some((_, &ch)) if ch as char != '\n' && ch as char != '\r' => {}
                            _ => break 'comment_or,
                        }
                    }
                }
                (_, &ch) if ch as char == '+' && self.expect_ch('+') => {
                    self.cur_skip_bytes += 2;
                    return Some(Ok(LexToken::Increment));
                }
                (_, &ch) if ch as char == '+' && self.expect_ch('=') => {
                    self.cur_skip_bytes += 2;
                    return Some(Ok(LexToken::AddInPlace));
                }
                (_, &ch) if ch as char == '-' && self.expect_ch('-') => {
                    self.cur_skip_bytes += 2;
                    return Some(Ok(LexToken::Decrement));
                }
                (_, &ch) if ch as char == '-' && self.expect_ch('-') => {
                    self.cur_skip_bytes += 2;
                    return Some(Ok(LexToken::Decrement));
                }
                (_, &ch) if ch as char == '-' && self.expect_ch('=') => {
                    self.cur_skip_bytes += 2;
                    return Some(Ok(LexToken::SubInPlace));
                }
                (_, &ch) if ch as char == '-' && self.expect_ch('>') => {
                    self.cur_skip_bytes += 2;
                    return Some(Ok(LexToken::Arrow));
                }
                (_, &ch) if ch as char == '&' && self.expect_ch('&') => {
                    self.cur_skip_bytes += 2;
                    return Some(Ok(LexToken::And));
                }
                (_, &ch) if ch as char == '&' && self.expect_ch('=') => {
                    self.cur_skip_bytes += 2;
                    return Some(Ok(LexToken::AndInPlace));
                }
                (_, &ch) if ch as char == '|' && self.expect_ch('|') => {
                    self.cur_skip_bytes += 2;
                    return Some(Ok(LexToken::Or));
                }
                (_, &ch) if ch as char == '|' && self.expect_ch('=') => {
                    self.cur_skip_bytes += 2;
                    return Some(Ok(LexToken::OrInPlace));
                }
                (_, &ch) if ch as char == '=' && self.expect_ch('=') => {
                    self.cur_skip_bytes += 2;
                    return Some(Ok(LexToken::Equ));
                }
                (_, &ch) if ch as char == '=' && self.expect_ch('>') => {
                    self.cur_skip_bytes += 2;
                    return Some(Ok(LexToken::EquArrow));
                }
                (_, &ch) if ch as char == '!' && self.expect_ch('=') => {
                    self.cur_skip_bytes += 2;
                    return Some(Ok(LexToken::Nequ));
                }
                (_, &ch) if ch as char == '^' && self.expect_ch('=') => {
                    self.cur_skip_bytes += 2;
                    return Some(Ok(LexToken::XorInPlace));
                }
                (_, &ch) if ch as char == '%' && self.expect_ch('=') => {
                    self.cur_skip_bytes += 2;
                    return Some(Ok(LexToken::ModInPlace));
                }
                (_, &ch) if ch as char == '*' && self.expect_ch('=') => {
                    self.cur_skip_bytes += 2;
                    return Some(Ok(LexToken::MulInPlace));
                }
                (_, &ch) if ch as char == '/' && self.expect_ch('=') => {
                    self.cur_skip_bytes += 2;
                    return Some(Ok(LexToken::DivInPlace));
                }
                (_, &ch) if ch as char == '<' && self.expect_ch('=') => {
                    self.cur_skip_bytes += 2;
                    return Some(Ok(LexToken::Lequ));
                }
                (_, &ch) if ch as char == '<' && self.expect_ch('<') => {
                    self.cur_skip_bytes += 2;
                    return Some(Ok(LexToken::Shl));
                }
                (_, &ch) if ch as char == '>' && self.expect_ch('=') => {
                    self.cur_skip_bytes += 2;
                    return Some(Ok(LexToken::Gequ));
                }
                (_, &ch) if ch as char == '>' && self.expect_ch('>') => {
                    self.cur_skip_bytes += 2;
                    return Some(Ok(LexToken::Shr));
                }
                (open_dq, &ch) if ch as char == '"' => {
                    // full word is <skip bytes>open_dq + 1..close_dq - 1
                    self.cur_stream = &self.cur_stream[self.cur_skip_bytes + 1..];
                    self.cur_skip_bytes = 0;
                    let mut sp = open_dq + 1;
                    let start = self.store.len();
                    loop {
                        match its.next() {
                            Some((close_dq, &ch)) if ch as char == '"' => {
                                let (upto, rest) = self.cur_stream.split_at(close_dq - sp);
                                self.cur_stream = rest;
                                self.store
                                    .extend(unsafe { std::str::from_utf8_unchecked(upto) }.chars());
                                let end = self.store.len();
                                self.cur_offset.0 += close_dq - 1;
                                self.cur_skip_bytes += 1;

                                return Some(Ok(LexToken::DoubleQuStr(
                                    StillMutatingStore { start, end },
                                )));
                            }
                            Some((escape_ch, &ch)) if ch as char == '\\' => {
                                if escape_ch > sp + 1 {
                                    let (upto, rest) = self.cur_stream.split_at(escape_ch - sp + 1);
                                    self.cur_stream = rest;
                                    self.store.extend(
                                        unsafe { std::str::from_utf8_unchecked(upto) }.chars(),
                                    );
                                    sp = escape_ch + 2;
                                }
                                match its.next() {
                                    Some((esc_ch_code, &ch)) => {
                                        // self.cur_stream.split_at(cur)
                                        self.store.push(match ch {
                                            b'\\' => b'\\',
                                            b'\'' => b'\'',
                                            b'"' => b'"',
                                            b't' => b'\t',
                                            b'n' => b'\n',
                                            b'r' => b'\r',
                                            b'0' => b'\0',
                                            b'x' | b'X' => todo!("hex lit"), // @TODO hex constants
                                            b'u' => todo!("unicode lit"), // @TODO unicode constants
                                            ch => {
                                                return Some(Err(LexError::UnknownEscCh {
                                                    at: self.file_loc(ByteOffset(
                                                        esc_ch_code + self.cur_offset.0,
                                                    ))?,
                                                    ch: ch as char,
                                                }))
                                            }
                                        }
                                            as char);
                                    }
                                    None => {
                                        return Some(Err(LexError::ParseLitStr {
                                            at: self
                                                .file_loc(ByteOffset(sp + self.cur_offset.0))?,
                                            expected: '\0',
                                        }))
                                    }
                                }
                            }
                            None => {
                                return Some(Err(LexError::ParseLitStr {
                                    at: self.file_loc(ByteOffset(open_dq + self.cur_offset.0))?,
                                    expected: '"',
                                }))
                            }
                            _ => {}
                        }
                    }
                }
                (ident_start, &ch)
                    if ch.is_ascii_alphabetic()
                        || ch as char == '_'
                        || ch as char == '$'
                        || ch >= 128 =>
                {
                    self.cur_stream = &self.cur_stream[self.cur_skip_bytes..];
                    self.cur_skip_bytes = 0;
                    let ident_end = loop {
                        match its.next()? {
                            (_, &ch)
                                if ch.is_ascii_alphabetic()
                                    || ch.is_ascii_digit()
                                    || ch as char == '_'
                                    || ch as char == '$'
                                    || ch >= 128 => {}
                            (off_we, _) => break off_we,
                        }
                    };

                    let (tok, rest) = self.cur_stream.split_at(ident_end - ident_start);

                    self.cur_offset.0 += ident_end;

                    self.cur_stream = rest;

                    let start = self.store.len();
                    self.store
                        .extend(unsafe { std::str::from_utf8_unchecked(tok) }.chars());
                    let end = self.store.len();

                    return Some(Ok(LexToken::Ident(StillMutatingStore {
                        start,
                        end,
                    })));
                }
                (_, &ch) => {
                    self.cur_skip_bytes += 1;
                    return Some(Ok(LexToken::Punct(ch)));
                }
            }
            self.cur_offset.0 += 1;
        }
    }
}

#[derive(Debug)]
pub enum LexError {
    ParseLitStr { at: FileLoc, expected: char },
    UnknownEscCh { at: FileLoc, ch: char },
}

#[derive(Debug)]
pub struct Lexer<'a, 's> {
    store: &'s mut String,
    input_stream: &'a [u8],
    cur_stream: &'a [u8],
    cur_offset: ByteOffset,
    cur_skip_bytes: usize,
}

impl<'a, 's> Lexer<'a, 's> {
    pub fn new(input_stream: &'a str, store: &'s mut String) -> Self {
        Self {
            store,
            input_stream: input_stream.as_bytes(),
            cur_stream: input_stream.as_bytes(),
            cur_offset: ByteOffset(0),
            cur_skip_bytes: 0,
        }
    }

    pub fn file_loc(&self, offset: ByteOffset) -> Option<FileLoc> {
        if offset.0 >= self.input_stream.len() {
            return None;
        }
        let (mut i, mut row, mut col) = (0, 0, 0);
        while i < offset.0 {
            match unsafe { self.input_stream.get_unchecked(i) } {
                &ch if ch as char == '\n' || ch as char == '\r' => {
                    match self.input_stream.get(i + 1) {
                        Some(&ch1) if ch as char == '\r' && ch1 as char == '\n' => i += 1,
                        _ => {}
                    }
                    row += 1;
                    col = 0;
                }
                _ => col += 1,
            }
            i += 1;
        }
        Some(FileLoc { row, col })
    }

    pub fn peek_ch(&self) -> Option<u8> {
        self.input_stream.get(self.cur_offset.0 + 1).copied()
    }

    pub fn expect_ch(&self, expect: char) -> bool {
        self.peek_ch().filter(|&ch| ch as char == expect).is_some()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct FileLoc {
    pub row: usize,
    pub col: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct ByteOffset(pub usize);

#[derive(Debug, Clone, Copy)]
pub struct StillMutatingStore { start: usize, end: usize }

impl StillMutatingStore {
    fn upgrade<'s>(&self, store: &'s String) -> Option<&'s str> {
        store.get(self.start..self.end)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum LexToken<T> {
    Punct(u8),
    IntLit,
    FloatLit,
    Ident(T),
    DoubleQuStr(T),
    CharLit,
    Equ,
    Nequ,
    Lequ,
    Gequ,
    And,
    Or,
    Shl,
    Shr,
    Increment,
    Decrement,
    AddInPlace,
    SubInPlace,
    MulInPlace,
    DivInPlace,
    ModInPlace,
    AndInPlace,
    OrInPlace,
    XorInPlace,
    Arrow,
    EquArrow,
    ShlInPlace,
    ShrInPlace,
    FirstUnused,
}
