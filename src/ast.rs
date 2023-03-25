pub type Block = Vec<Statement>;
pub type Identifier = String;

pub enum Statement {
    Expression(Expression),
    Import(ImportStatement),
    Exit,
    Stop(StopStatement),
    Assignment(AssignmentStatement),
    Break(BreakStatement),
    Continue,
}

pub enum ImportDestination {
    Into(Expression),
    As(String),
}

pub struct ImportStatement {
    pub name: Identifier,
    pub destination: ImportDestination,
}

pub enum StopDirection {
    Reading,
    Writing,
    Both,
}

pub struct StopStatement {
    pub mode: StopDirection,
    pub target: Expression,
}

pub struct BreakStatement {
    pub argument: Option<Expression>,
}

pub struct AssignmentStatement {
    pub left_side: Pattern,
    pub right_side: Expression,
}

pub enum BinaryOperator {
    ListIndex,
    RecordIndex,
    RemoveListIndex,
    RemoveRecordIndex,
    Plus,
    Minus,
    Times,
    Divide,
    And,
    Or,
    StreamLeftOnce,
    StreamRightOnce,
    StreamLeftAll,
    StreamRightAll,
}

pub struct BinaryExpression {
    pub operator: BinaryOperator,
    pub left_side: Box<Expression>,
    pub right_side: Box<Expression>,
}

pub enum UnaryOperator {
    Not,
    Negate,
}

pub struct UnaryExpression {
    pub operator: UnaryOperator,
    pub operand: Box<Expression>,
}

// `$`
pub enum SimpleStreamExpression {
    Pattern(Pattern),
    Expression(Box<Expression>),
}

pub struct TupleBuilderExpression {
    pub symbol: String,
    pub contents: Vec<Expression>,
}

pub struct ListBuilderExpression {
    pub contents: Vec<Expression>,
}

pub struct RecordBuilderExpression {
    pub contents: Vec<(String, Expression)>,
}

pub enum ForLoopParallelismPolicy {
    Serial,
    Parallel,
}

pub struct ForLoop {
    pub parallelism_policy: ForLoopParallelismPolicy,
    pub loop_variable: Identifier,
    pub stream_expression: Box<Expression>,
    pub body: Block,
}

pub struct MatchExpression {
    pub scrutinee: Box<Expression>,
    pub cases: Vec<MatchCase>,
}

pub struct MatchCase {
    pub pattern: Pattern,
    pub expression: Expression,
}

pub enum Expression {
    Binary(BinaryExpression),
    Unary(UnaryExpression),
    Identifier(Identifier),
    ListBuilder(ListBuilderExpression),
    RecordBuilder(RecordBuilderExpression),
    StringLiteral(String),
    NumericLiteral(f64),
    BooleanLiteral(bool),
    TupleBuilder(TupleBuilderExpression),
    SimpleStream(SimpleStreamExpression),
    InStream,
    OutStream,
    ItemVariable, // `%`
    DoBlock(Block),
    RepeatBlock(Block),
    StreamBlock(Block),
    ShorthandStreamBlock(Block),
    ForLoop(ForLoop),
    Match(MatchExpression),
}

pub enum Pattern {
    Identifier(Identifier),
}
