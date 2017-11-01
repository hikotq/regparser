use std::cell::RefCell;
use std::option::Option::{Some, None};

struct Lexer {
    string_list: RefCell<Vec<String>>,
}

impl Lexer {
    fn new(regex: &str) -> Lexer {
        let regex = regex
            .chars()
            .map(|c| c.to_string())
            .collect::<Vec<String>>();
        let lexer = Lexer { string_list: RefCell::new(regex) };
        lexer
    }

    fn scan(&self) -> Token {
        use self::TokenType::*;
        if self.string_list.borrow().is_empty() {
            return Token {
                value: None,
                kind: EOF,
            };
        }
        let ch = self.string_list.borrow_mut().remove(0);
        match &*ch {
            "|" => Token {
                value: Some(ch),
                kind: OpUnion,
            },
            "(" => Token {
                value: Some(ch),
                kind: Lparen,
            },
            ")" => Token {
                value: Some(ch),
                kind: Rparen,
            },
            "*" => Token {
                value: Some(ch),
                kind: OpStar,
            },
            "!" => Token {
                value: Some(ch),
                kind: OpNegation,
            },
            _ => Token {
                value: Some(ch),
                kind: Literal,
            },
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
enum TokenType {
    OpUnion,
    OpStar,
    OpConcat,
    OpNegation,
    Literal,
    Lparen,
    Rparen,
    EOF,
}

struct Node {
    token: Token,
    lhs: Option<Box<Node>>,
    rhs: Option<Box<Node>>,
}

impl Node {
    fn star(operand: Box<Node>) -> Box<Node> {
        Box::new(Node {
            token: Token {
                value: None,
                kind: TokenType::OpStar,
            },
            lhs: Some(operand),
            rhs: None,
        })
    }

    fn union(operand1: Box<Node>, operand2: Box<Node>) -> Box<Node> {
        Box::new(Node {
            token: Token {
                value: None,
                kind: TokenType::OpUnion,
            },
            lhs: Some(operand1),
            rhs: Some(operand2),
        })
    }

    fn concat(operand1: Box<Node>, operand2: Box<Node>) -> Box<Node> {
        Box::new(Node {
            token: Token {
                value: None,
                kind: TokenType::OpConcat,
            },
            lhs: Some(operand1),
            rhs: Some(operand2),
        })
    }

    fn negation(operand: Box<Node>) -> Box<Node> {
        Box::new(Node {
            token: Token {
                value: None,
                kind: TokenType::OpNegation,
            },
            lhs: Some(operand),
            rhs: None,
        })
    }

    fn literal(ch: String) -> Box<Node> {
        Box::new(Node {
            token: Token {
                value: Some(ch),
                kind: TokenType::Literal,
            },
            lhs: None,
            rhs: None,
        })
    }

    fn print(&self, depth: usize) {
        for _ in 0..depth {
            print!(" ");
        }
        print!("{:?}", self.token.kind);
        println!("");
        if let Some(ref node) = self.lhs {
            node.print(depth + 1);
        }
        if let Some(ref node) = self.rhs {
            node.print(depth + 1);
        }
    }
}

struct Token {
    value: Option<String>,
    kind: TokenType,
}

struct Parser {
    lexer: Lexer,
    look: RefCell<Token>,
}

impl Parser {
    fn new(lexer: Lexer) -> Parser {
        use self::TokenType::*;
        let init_token = Token {
            value: None,
            kind: EOF,
        };
        let parser = Parser {
            lexer: lexer,
            look: RefCell::new(init_token),
        };
        parser.scan();
        parser
    }

    fn consume(&self, tag: TokenType) {
        if self.look.borrow().kind != tag {
            panic!("syntax error");
        }
        self.scan();
    }

    fn scan(&self) {
        *self.look.borrow_mut() = self.lexer.scan();
    }

    fn factor(&self) -> Box<Node> {
        if self.look.borrow().kind == TokenType::Lparen {
            //factor -> '(' subexpr ')'
            self.consume(TokenType::Lparen);
            let node = self.subexpr();
            self.consume(TokenType::Rparen);
            node
        } else if self.look.borrow().kind == TokenType::OpNegation {
            let node = self.negation();
            node
        } else {
            let ch: String = self.look.borrow_mut().value.as_ref().unwrap().clone();
            let node = Node::literal(ch);
            self.consume(TokenType::Literal);
            node
        }
    }

    fn negation(&self) -> Box<Node> {
        self.consume(TokenType::OpNegation);
        let operand = self.factor();
        let node = Node::negation(operand);
        node
    }

    fn star(&self) -> Box<Node> {
        let mut node = self.factor();
        if self.look.borrow().kind == TokenType::OpStar {
            self.consume(TokenType::OpStar);
            node = Node::star(node)
        }
        node
    }

    fn subseq(&self) -> Box<Node> {
        let node1 = self.star();
        if self.look.borrow().kind == TokenType::Lparen ||
            self.look.borrow().kind == TokenType::Literal
        {
            //subseq -> star subseq
            let node2 = self.subseq();
            let node = Node::concat(node1, node2);
            node
        } else {
            //subseq -> star
            node1
        }
    }

    fn seq(&self) -> Box<Node> {
        if self.look.borrow().kind == TokenType::Lparen ||
            self.look.borrow().kind == TokenType::OpNegation ||
            self.look.borrow().kind == TokenType::Literal
        {
            self.subseq()
        } else {
            Node::literal("".to_string())
        }

        //match self.look.borrow().kind {
        //    //seq -> subseq
        //    TokenType::Lparen | TokenType::OpNegation | TokenType::Literal => self.subseq(),
        //    //seq -> ''
        //    _ => Node::literal("".to_string()),
        //}
    }

    fn subexpr(&self) -> Box<Node> {
        //subexpr -> seq '|' subexpr | seq
        let mut node = self.seq();
        if self.look.borrow().kind == TokenType::OpUnion {
            self.consume(TokenType::OpUnion);
            let node2 = self.subexpr();
            node = Node::union(node, node2);
        }
        node
    }

    fn expr(&self) -> Box<Node> {
        //expression -> subexpr EOF
        let node = self.subexpr();
        self.consume(TokenType::EOF);
        node
    }
}

#[test]
fn regex_parse_star() {
    let regex = "001*";
    let lexer = Lexer::new(regex);
    let parser = Parser::new(lexer);
    let syntax_tree: Box<Node> = parser.expr();
}

#[test]
fn regex_parse_union() {
    let regex = "(1|01)001";
    let lexer = Lexer::new(regex);
    let parser = Parser::new(lexer);
    let syntax_tree: Box<Node> = parser.expr();
}

#[test]
fn regex_parse_negation() {
    let regex = "(!(001.*|.*01)221)";
    let lexer = Lexer::new(regex);
    let parser = Parser::new(lexer);
    let syntax_tree: Box<Node> = parser.expr();
}
