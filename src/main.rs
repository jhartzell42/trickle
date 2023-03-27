mod ast;

#[macro_use] extern crate lalrpop_util;

lalrpop_mod!(pub grammar); // synthesized by LALRPOP

#[test]
fn grammar() {
    assert_eq!(grammar::TermParser::new().parse("22"), Ok(22));
    assert_eq!(grammar::TermParser::new().parse("(22)"), Ok(22));
    assert_eq!(grammar::TermParser::new().parse("((((22))))"), Ok(22));
    assert!(grammar::TermParser::new().parse("((22)").is_err());
}

fn main() {
    println!("Hello, world!");
}
