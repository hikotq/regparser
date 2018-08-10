use std::cell::{Cell, RefCell};
use std::collections::VecDeque;
use std::option::Option::{None, Some};

#[derive(PartialEq, Eq, Debug)]
enum TokenType {
    OpUnion,
    OpStar,
    OpNegation,
    Literal,
    Dot,
    Lparen,
    Rparen,
    EOF,
}

pub struct Token {
    value: Option<u8>,
    kind: TokenType,
}

pub struct Lexer {
    string_list: RefCell<Vec<String>>,
    token_queue: RefCell<VecDeque<Token>>,
}

impl Lexer {
    pub fn new(regex: &str) -> Lexer {
        let regex = regex
            .chars()
            .map(|c| c.to_string())
            .collect::<Vec<String>>();
        let lexer = Lexer {
            string_list: RefCell::new(regex),
            token_queue: RefCell::new(VecDeque::new()),
        };
        lexer
    }

    pub fn scan(&self) -> Token {
        use self::TokenType::*;
        if self.string_list.borrow().is_empty() {
            return Token {
                value: None,
                kind: EOF,
            };
        }
        if !self.token_queue.borrow().is_empty() {
            self.token_queue.borrow_mut().pop_front().unwrap()
        } else {
            let s = self.string_list.borrow_mut().remove(0);
            match &*s {
                "|" => self.token_queue.borrow_mut().push_back(Token {
                    value: None,
                    kind: OpUnion,
                }),
                "(" => self.token_queue.borrow_mut().push_back(Token {
                    value: None,
                    kind: Lparen,
                }),
                ")" => self.token_queue.borrow_mut().push_back(Token {
                    value: None,
                    kind: Rparen,
                }),
                "*" => self.token_queue.borrow_mut().push_back(Token {
                    value: None,
                    kind: OpStar,
                }),
                "!" => self.token_queue.borrow_mut().push_back(Token {
                    value: None,
                    kind: OpNegation,
                }),
                "." => self.token_queue.borrow_mut().push_back(Token {
                    value: None,
                    kind: Dot,
                }),
                _ => for b in s.as_bytes() {
                    self.token_queue.borrow_mut().push_back(Token {
                        value: Some(*b),
                        kind: Literal,
                    });
                },
            };
            self.token_queue.borrow_mut().pop_front().unwrap()
        }
    }
}

use std::marker::PhantomData;
struct Empty;
struct Filled;

pub struct NodeBuilder<_NodeType> {
    node_type: Option<NodeType>,
    node_type_state: PhantomData<_NodeType>,
    value: Option<u8>,
    lhs: Option<Box<Node>>,
    rhs: Option<Box<Node>>,
    group_id: Option<u32>,
    isPrefix: bool,
    isSuffix: bool,
}

impl NodeBuilder<Empty> {
    pub fn new() -> Self {
        NodeBuilder {
            node_type: None,
            node_type_state: PhantomData,
            value: None,
            lhs: None,
            rhs: None,
            group_id: None,
            isPrefix: false,
            isSuffix: false,
        }
    }

    pub fn node_type(self, node_type: NodeType) -> NodeBuilder<Filled> {
        NodeBuilder {
            node_type: Some(node_type),
            node_type_state: PhantomData,
            value: self.value,
            lhs: self.lhs,
            rhs: self.rhs,
            group_id: self.group_id,
            isPrefix: self.isPrefix,
            isSuffix: self.isSuffix,
        }
    }
}

impl<_NodeType> NodeBuilder<_NodeType> {
    pub fn value(mut self, value: Option<u8>) -> Self {
        self.value = value;
        self
    }

    pub fn lhs(mut self, lhs: Option<Box<Node>>) -> Self {
        self.lhs = lhs;
        self
    }

    pub fn rhs(mut self, rhs: Option<Box<Node>>) -> Self {
        self.rhs = rhs;
        self
    }

    pub fn group_id(mut self, group_id: Option<u32>) -> Self {
        self.group_id = group_id;
        self
    }

    pub fn isPrefix(mut self, isPrefix: bool) -> Self {
        self.isPrefix = isPrefix;
        self
    }

    pub fn isSuffix(mut self, isSuffix: bool) -> Self {
        self.isSuffix = isSuffix;
        self
    }
}

impl NodeBuilder<Filled> {
    pub fn build(self) -> Node {
        Node {
            node_type: self.node_type.unwrap(),
            value: self.value,
            lhs: self.lhs,
            rhs: self.rhs,
            group_id: self.group_id,
            isPrefix: Cell::new(self.isPrefix),
            isSuffix: Cell::new(self.isSuffix),
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum NodeType {
    Group,
    OpUnion,
    OpNegation,
    OpStar,
    OpConcat,
    Dot,
    Literal,
}

pub struct Node {
    pub node_type: NodeType,
    pub value: Option<u8>,
    pub lhs: Option<Box<Node>>,
    pub rhs: Option<Box<Node>>,
    pub group_id: Option<u32>,
    isPrefix: Cell<bool>,
    isSuffix: Cell<bool>,
}

impl Node {
    fn group(operand: Box<Node>, id: u32) -> Box<Node> {
        Box::new(
            NodeBuilder::new()
                .node_type(NodeType::Group)
                .lhs(Some(operand))
                .group_id(Some(id))
                .build(),
        )
    }

    fn star(operand: Box<Node>) -> Box<Node> {
        Box::new(
            NodeBuilder::new()
                .node_type(NodeType::OpStar)
                .lhs(Some(operand))
                .build(),
        )
    }

    fn union(operand1: Box<Node>, operand2: Box<Node>) -> Box<Node> {
        Box::new(
            NodeBuilder::new()
                .node_type(NodeType::OpUnion)
                .lhs(Some(operand1))
                .rhs(Some(operand2))
                .build(),
        )
    }

    fn concat(operand1: Box<Node>, operand2: Box<Node>) -> Box<Node> {
        Box::new(
            NodeBuilder::new()
                .node_type(NodeType::OpConcat)
                .lhs(Some(operand1))
                .rhs(Some(operand2))
                .build(),
        )
    }

    fn negation(operand: Box<Node>) -> Box<Node> {
        Box::new(
            NodeBuilder::new()
                .node_type(NodeType::OpNegation)
                .lhs(Some(operand))
                .build(),
        )
    }

    fn dot() -> Box<Node> {
        Box::new(NodeBuilder::new().node_type(NodeType::Dot).build())
    }

    fn literal(ch: Option<u8>) -> Box<Node> {
        Box::new(
            NodeBuilder::new()
                .node_type(NodeType::Literal)
                .value(ch)
                .build(),
        )
    }

    pub fn make_regex(&self) -> String {
        let regex: String = Node::make_regex_recursion(self);
        regex
    }

    pub fn make_regex_recursion(syntax_tree: &Node) -> String {
        println!("{:?}", syntax_tree.node_type);
        let &Node {
            ref lhs, ref rhs, ..
        } = syntax_tree;
        let lhs: Option<&Box<Node>> = lhs.as_ref();
        let rhs: Option<&Box<Node>> = rhs.as_ref();
        let regex = match syntax_tree.node_type {
            NodeType::Group => {
                let regex = Node::make_regex_recursion(lhs.unwrap());
                let regex = "(".to_string() + &regex + ")";
                regex
            }
            NodeType::OpNegation => {
                let op_negation = "!";
                let regex = Node::make_regex_recursion(lhs.unwrap());
                let regex = if lhs.unwrap().node_type == NodeType::OpUnion {
                    op_negation.to_string() + &regex
                } else {
                    op_negation.to_string() + "(" + &regex + ")"
                };
                regex.to_string()
            }
            NodeType::OpUnion => {
                let regex1 = Node::make_regex_recursion(lhs.unwrap());
                let op_union = "|";
                let regex2 = Node::make_regex_recursion(rhs.unwrap());
                let regex = "(".to_string() + &regex1 + op_union + &regex2 + ")";
                regex.to_string()
            }
            NodeType::OpConcat => {
                let regex1 = Node::make_regex_recursion(lhs.unwrap());
                let regex2 = Node::make_regex_recursion(rhs.unwrap());
                let regex = regex1 + &regex2;
                regex.to_string()
            }
            NodeType::OpStar => {
                let regex = Node::make_regex_recursion(lhs.unwrap());
                let op_star = "*";
                let regex = regex + op_star;
                regex.to_string()
            }
            NodeType::Dot => {
                let regex = ".".to_string();
                regex
            }
            _ => {
                if let Some(regex) = syntax_tree.value.as_ref() {
                    (*regex as char).to_string()
                } else {
                    "".to_string()
                }
            }
        };
        let regex = if syntax_tree.isPrefix.get() {
            "^".to_string() + &regex
        } else {
            regex
        };
        let regex = if syntax_tree.isSuffix.get() {
            regex + "$"
        } else {
            regex
        };
        regex
    }

    pub fn print(&self, depth: usize) {
        for _ in 0..depth {
            print!(" ");
        }
        print!("{:?}", self.node_type);
        if self.node_type == NodeType::Group {
            print!(" {}", self.group_id.unwrap());
        }
        println!("");
        if let Some(ref node) = self.lhs {
            node.print(depth + 1);
        }
        if let Some(ref node) = self.rhs {
            node.print(depth + 1);
        }
    }
}

pub struct Tree {
    pub root: Option<Box<Node>>,
}

impl Tree {
    pub fn make_regex(&self) -> String {
        if let Some(ref node) = self.root {
            node.make_regex()
        } else {
            panic!("No tree has any nodes");
        }
    }

    pub fn print(&self) {
        if let Some(ref node) = self.root {
            node.print(0);
        } else {
            panic!("No tree has any nodes");
        }
    }
}

pub struct Parser {
    lexer: Lexer,
    look: RefCell<Token>,
    group_count: Cell<u32>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        use self::TokenType::*;
        let init_token = Token {
            value: None,
            kind: EOF,
        };
        let mut parser = Parser {
            lexer: lexer,
            look: RefCell::new(init_token),
            group_count: Cell::new(0),
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
            //factor -> '(' group ')'
            self.consume(TokenType::Lparen);
            let node = self.group();
            self.consume(TokenType::Rparen);
            node
        } else if self.look.borrow().kind == TokenType::OpNegation {
            let node = self.negation();
            node
        } else if self.look.borrow().kind == TokenType::Dot {
            let node = Node::dot();
            self.consume(TokenType::Dot);
            node
        } else {
            let ch: u8 = self.look.borrow_mut().value.as_ref().unwrap().clone();
            let node = Node::literal(Some(ch));
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
        if self.look.borrow().kind == TokenType::Lparen
            || self.look.borrow().kind == TokenType::OpNegation
            || self.look.borrow().kind == TokenType::Literal
            || self.look.borrow().kind == TokenType::Dot
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
        if self.look.borrow().kind == TokenType::Lparen
            || self.look.borrow().kind == TokenType::OpNegation
            || self.look.borrow().kind == TokenType::Literal
            || self.look.borrow().kind == TokenType::Dot
        {
            self.subseq()
        } else {
            Node::literal(None)
        }

        //match self.look.borrow().kind {
        //    //seq -> subseq
        //    TokenType::Lparen | TokenType::OpNegation | TokenType::Literal => self.subseq(),
        //    //seq -> ''
        //    _ => Node::literal("".to_string()),
        //}
    }

    fn group(&self) -> Box<Node> {
        //group -> subexpr
        self.group_count.set(self.group_count.get() + 1);
        let group_id = self.group_count.get();
        let operand = self.subexpr();
        let node = Node::group(operand, group_id);
        node
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

    pub fn struct_syntax_tree(&self) -> Tree {
        let node = self.expr();
        let tree = Tree { root: Some(node) };
        tree
    }
}

#[test]
fn regex_parse_star() {
    let regex = "001*";
    let lexer = Lexer::new(regex);
    let parser = Parser::new(lexer);
    let syntax_tree: Tree = parser.struct_syntax_tree();
    assert!(regex == syntax_tree.make_regex());
}

#[test]
fn regex_parse_union() {
    let regex = "(1|01)001";
    let lexer = Lexer::new(regex);
    let parser = Parser::new(lexer);
    let syntax_tree: Tree = parser.struct_syntax_tree();
    assert!("((1|01))001" == syntax_tree.make_regex());
}
