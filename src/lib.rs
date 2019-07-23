use std::io::{stdout, Write};

use proc_macro2::{Delimiter, Spacing, Span, TokenStream, TokenTree};
use term::terminfo::TermInfo;
use term::{Terminal, TerminfoTerminal};

pub struct Pretty {
    inner: TokenStream,
}

struct Printer<'a, T> {
    terminal: &'a mut T,
    inline: bool,
    colored: bool,
    depth: u32,
}

impl Pretty {
    pub fn new(token_stream: TokenStream) -> Self {
        Pretty {
            inner: token_stream,
        }
    }

    pub fn print(self) {
        let out = stdout();
        let out = out.lock();

        if cfg!(procmacro2_semver_exempt) && atty::is(atty::Stream::Stdout) {
            if let Ok(ti) = TermInfo::from_env() {
                return self.print_to_terminal(&mut TerminfoTerminal::new_with_terminfo(out, ti));
            }
        }

        self.print_to(out)
    }

    pub fn print_to<W: Write>(self, out: W) {
        let ti = TermInfo {
            names: Default::default(),
            bools: Default::default(),
            numbers: Default::default(),
            strings: Default::default(),
        };
        self.print_to_terminal(&mut TerminfoTerminal::new_with_terminfo(out, ti))
    }

    pub fn print_to_terminal<T: Terminal>(self, terminal: &mut T) {
        Printer {
            terminal,
            inline: false,
            colored: false,
            depth: 0,
        }
        .print_tokens(self.inner);
        terminal.reset().ok();
    }
}

impl<'a, T: Terminal> Printer<'a, T> {
    fn print_tokens(&mut self, tokens: TokenStream) {
        for tt in tokens {
            self.print(tt);
        }
    }

    fn print(&mut self, tt: TokenTree) {
        self.set_color(tt.span());

        match tt {
            TokenTree::Group(group) => {
                let inline_original = self.inline;

                match group.delimiter() {
                    Delimiter::Brace => {
                        self.terminal.write_all(b"{").unwrap();
                        self.newline();
                        self.depth += 1;
                    }
                    Delimiter::Parenthesis => {
                        self.inline = true;
                        self.terminal.write_all(b"(").unwrap();
                    }
                    Delimiter::Bracket => {
                        self.inline = true;
                        self.terminal.write_all(b"[").unwrap();
                    }
                    Delimiter::None => {}
                }

                self.print_tokens(group.stream());
                self.inline = inline_original;
                self.set_color(group.span());

                match group.delimiter() {
                    Delimiter::Brace => {
                        self.depth -= 1;
                        self.newline();
                        self.terminal.write_all(b"}").unwrap();
                        self.newline();
                    }
                    Delimiter::Parenthesis => self.terminal.write_all(b")").unwrap(),
                    Delimiter::Bracket => self.terminal.write_all(b"]").unwrap(),
                    Delimiter::None => {}
                }
            }
            TokenTree::Ident(ident) => {
                write!(self.terminal, "{} ", ident).unwrap();
            }
            TokenTree::Punct(punct) => match punct.as_char() {
                ';' => {
                    self.terminal.write_all(b";").unwrap();
                    self.newline();
                }
                c => match punct.spacing() {
                    Spacing::Alone => write!(self.terminal, "{} ", c).unwrap(),
                    Spacing::Joint => write!(self.terminal, "{}", c).unwrap(),
                },
            },
            TokenTree::Literal(lit) => write!(self.terminal, "{}", lit).unwrap(),
        }
    }

    fn set_color(&mut self, span: Span) {
        if is_callsite(span) {
            if self.colored {
                self.terminal.reset().ok();
                self.colored = false;
            }
        } else {
            if !self.colored {
                self.terminal.fg(term::color::YELLOW).ok();
                self.colored = true;
            }
        }
    }

    fn newline(&mut self) {
        if !self.inline {
            self.terminal.write_all(b"\n").unwrap();
            for _ in 0..self.depth {
                self.terminal.write_all(b"    ").unwrap();
            }
        }
    }
}

cfg_if::cfg_if! {
    if #[cfg(procmacro2_semver_exempt)] {
        fn is_callsite(s: Span) -> bool {
            s.eq(&Span::call_site())
        }
    } else {
        fn is_callsite(_: Span) -> bool {
            true
        }
    }
}
