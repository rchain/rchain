//! Faithful Rust port of the RChain `graphz` module (a Graphviz DOT string builder).
//!
//! Mirrors `graphz/src/main/scala/coop/rchain/graphz/Graphz.scala`. The cats-effect `F[_]`
//! effect is simplified to synchronous accumulation (the async/effect model is reintroduced when
//! the node's runtime lands). Note: despite `build.sbt` declaring `dependsOn(shared)`, the Scala
//! `Graphz.scala` imports nothing from `shared`, so this crate is a leaf.

/// Accumulates serialized DOT output.
pub trait GraphSerializer {
    /// Append `str` followed by `suffix`.
    fn push(&mut self, str: &str, suffix: &str);

    /// Append `str` followed by a newline.
    fn push_line(&mut self, str: &str) {
        self.push(str, "\n");
    }
}

/// Accumulates into a single `String`.
#[derive(Default)]
pub struct StringSerializer {
    pub buf: String,
}

impl StringSerializer {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn into_string(self) -> String {
        self.buf
    }
}

impl GraphSerializer for StringSerializer {
    fn push(&mut self, str: &str, suffix: &str) {
        self.buf.push_str(str);
        self.buf.push_str(suffix);
    }
}

/// Accumulates one entry per push.
#[derive(Default)]
pub struct ListSerializer {
    pub buf: Vec<String>,
}

impl GraphSerializer for ListSerializer {
    fn push(&mut self, str: &str, suffix: &str) {
        self.buf.push(format!("{str}{suffix}"));
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GraphType {
    Graph,
    DiGraph,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GraphShape {
    Circle,
    DoubleCircle,
    DoubleOctagon,
    Box,
    PlainText,
    Msquare,
    Record,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GraphRank {
    Same,
    Min,
    Source,
    Max,
    Sink,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GraphRankDir {
    TB,
    BT,
    LR,
    RL,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GraphStyle {
    Solid,
    Bold,
    Filled,
    Invis,
    Dotted,
    Dashed,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GraphArrowType {
    NormalArrow,
    InvArrow,
    NoneArrow,
}

impl GraphShape {
    pub fn show(&self) -> &'static str {
        match self {
            GraphShape::Circle => "circle",
            GraphShape::DoubleCircle => "doublecircle",
            GraphShape::DoubleOctagon => "doubleoctagon",
            GraphShape::Box => "box",
            GraphShape::PlainText => "plaintext",
            GraphShape::Msquare => "Msquare",
            GraphShape::Record => "record",
        }
    }
}

impl GraphStyle {
    /// Lowercased variant name (the Scala `smallToString`).
    pub fn show(&self) -> &'static str {
        match self {
            GraphStyle::Solid => "solid",
            GraphStyle::Bold => "bold",
            GraphStyle::Filled => "filled",
            GraphStyle::Invis => "invis",
            GraphStyle::Dotted => "dotted",
            GraphStyle::Dashed => "dashed",
        }
    }
}

impl GraphRank {
    /// Lowercased variant name (the Scala `smallToString`).
    pub fn show(&self) -> &'static str {
        match self {
            GraphRank::Same => "same",
            GraphRank::Min => "min",
            GraphRank::Source => "source",
            GraphRank::Max => "max",
            GraphRank::Sink => "sink",
        }
    }
}

impl GraphRankDir {
    pub fn show(&self) -> &'static str {
        match self {
            GraphRankDir::TB => "TB",
            GraphRankDir::BT => "BT",
            GraphRankDir::LR => "LR",
            GraphRankDir::RL => "RL",
        }
    }
}

impl GraphArrowType {
    pub fn show(&self) -> &'static str {
        match self {
            GraphArrowType::NormalArrow => "normal",
            GraphArrowType::InvArrow => "inv",
            GraphArrowType::NoneArrow => "none",
        }
    }
}

/// The default node shape.
pub const DEFAULT_SHAPE: GraphShape = GraphShape::Circle;

const TAB: &str = "  ";

/// Builder options for [`Graphz::apply`].
#[derive(Default, Clone, Debug)]
pub struct GraphzOptions {
    pub subgraph: bool,
    pub comment: Option<String>,
    pub label: Option<String>,
    pub splines: Option<String>,
    pub rank: Option<GraphRank>,
    pub rankdir: Option<GraphRankDir>,
    pub style: Option<String>,
    pub color: Option<String>,
    pub graph: Vec<(String, String)>,
    pub node: Vec<(String, String)>,
    pub edge: Vec<(String, String)>,
}

/// A DOT (sub)graph being built.
pub struct Graphz {
    gtype: GraphType,
    t: String,
}

impl Graphz {
    /// The full builder (the Scala `Graphz.apply`).
    pub fn apply<S: GraphSerializer>(
        name: &str,
        gtype: GraphType,
        ser: &mut S,
        opts: &GraphzOptions,
    ) -> Graphz {
        let t = if opts.subgraph {
            format!("{TAB}{TAB}")
        } else {
            TAB.to_string()
        };
        if let Some(c) = &opts.comment {
            ser.push_line(&format!("// {c}"));
        }
        ser.push_line(&head(gtype, opts.subgraph, name));

        // The Scala `insert` uses `tab + tab` for subgraphs and `tab` otherwise.
        let indent = if opts.subgraph { "    " } else { "  " };
        insert(ser, indent, opts.label.as_deref(), |l| {
            format!("label = {}", quote(l))
        });
        insert(ser, indent, opts.style.as_deref(), |s| format!("style={s}"));
        insert(ser, indent, opts.color.as_deref(), |s| format!("color={s}"));
        insert(ser, indent, opts.rank.map(|r| r.show()), |r| {
            format!("rank={r}")
        });
        insert(ser, indent, opts.rankdir.map(|r| r.show()), |r| {
            format!("rankdir={r}")
        });
        insert(ser, indent, attr_mk_str(&opts.graph).as_deref(), |n| {
            format!("graph {n}")
        });
        insert(ser, indent, attr_mk_str(&opts.node).as_deref(), |n| {
            format!("node {n}")
        });
        insert(ser, indent, attr_mk_str(&opts.edge).as_deref(), |n| {
            format!("edge {n}")
        });
        insert(ser, indent, opts.splines.as_deref(), |s| {
            format!("splines={s}")
        });
        Graphz { gtype, t }
    }

    /// A top-level graph with no extra options.
    pub fn new<S: GraphSerializer>(name: &str, gtype: GraphType, ser: &mut S) -> Graphz {
        Self::apply(name, gtype, ser, &GraphzOptions::default())
    }

    /// A subgraph (the Scala `Graphz.subgraph`).
    pub fn subgraph<S: GraphSerializer>(
        name: &str,
        gtype: GraphType,
        ser: &mut S,
        label: Option<&str>,
        rank: Option<GraphRank>,
        rankdir: Option<GraphRankDir>,
        style: Option<&str>,
        color: Option<&str>,
    ) -> Graphz {
        let opts = GraphzOptions {
            subgraph: true,
            label: label.map(String::from),
            rank,
            rankdir,
            style: style.map(String::from),
            color: color.map(String::from),
            ..Default::default()
        };
        Self::apply(name, gtype, ser, &opts)
    }

    pub fn edge<S: GraphSerializer>(
        &self,
        src: &str,
        dst: &str,
        style: Option<GraphStyle>,
        arrow_head: Option<GraphArrowType>,
        constraint: Option<bool>,
        ser: &mut S,
    ) {
        let mut attrs: Vec<(String, String)> = Vec::new();
        if let Some(s) = style {
            attrs.push(("style".to_string(), s.show().to_string()));
        }
        if let Some(c) = constraint {
            attrs.push(("constraint".to_string(), c.to_string()));
        }
        if let Some(a) = arrow_head {
            attrs.push(("arrowhead".to_string(), a.show().to_string()));
        }
        let attr = attr_mk_str(&attrs)
            .map(|a| format!(" {a}"))
            .unwrap_or_default();
        let sep = match self.gtype {
            GraphType::Graph => " -- ",
            GraphType::DiGraph => " -> ",
        };
        ser.push_line(&format!("{}{}{}{}{}", self.t, quote(src), sep, quote(dst), attr));
    }

    #[allow(clippy::too_many_arguments)]
    pub fn node<S: GraphSerializer>(
        &self,
        name: &str,
        shape: GraphShape,
        style: Option<GraphStyle>,
        color: Option<&str>,
        border: Option<&str>,
        border_width: Option<i32>,
        label: Option<&str>,
        ser: &mut S,
    ) {
        let mut attrs: Vec<(String, String)> = Vec::new();
        if shape != DEFAULT_SHAPE {
            attrs.push(("shape".to_string(), shape.show().to_string()));
        }
        if let Some(c) = color {
            attrs.push(("fillcolor".to_string(), format!("\"{c}\"")));
        }
        if let Some(b) = border {
            attrs.push(("color".to_string(), format!("\"{b}\"")));
        }
        if let Some(w) = border_width {
            attrs.push(("penwidth".to_string(), w.to_string()));
        }
        if let Some(l) = label {
            attrs.push(("label".to_string(), l.to_string()));
        }
        if let Some(s) = style {
            attrs.push(("style".to_string(), s.show().to_string()));
        }
        let attr = attr_mk_str(&attrs)
            .map(|a| format!(" {a}"))
            .unwrap_or_default();
        ser.push_line(&format!("{}{}{}", self.t, quote(name), attr));
    }

    pub fn close<S: GraphSerializer>(&self, ser: &mut S) {
        let content = &self.t[TAB.len()..];
        let suffix = if content.is_empty() { "" } else { "\n" };
        ser.push(&format!("{content}}}"), suffix);
    }
}

fn head(gtype: GraphType, subgraph: bool, name: &str) -> String {
    let prefix = match (gtype, subgraph) {
        (_, true) => format!("{TAB}subgraph"),
        (GraphType::Graph, _) => "graph".to_string(),
        (GraphType::DiGraph, _) => "digraph".to_string(),
    };
    if name.is_empty() {
        format!("{prefix} {{")
    } else {
        format!("{prefix} \"{name}\" {{")
    }
}

fn quote(str: &str) -> String {
    if str.starts_with('"') {
        str.to_string()
    } else {
        format!("\"{str}\"")
    }
}

fn attr_mk_str(attrs: &[(String, String)]) -> Option<String> {
    if attrs.is_empty() {
        None
    } else {
        let inner = attrs
            .iter()
            .map(|(k, v)| format!("{k}={v}"))
            .collect::<Vec<_>>()
            .join(" ");
        Some(format!("[{inner}]"))
    }
}

fn insert<S: GraphSerializer>(
    ser: &mut S,
    indent: &str,
    opt: Option<&str>,
    f: impl FnOnce(&str) -> String,
) {
    if let Some(s) = opt {
        ser.push_line(&format!("{indent}{}", f(s)));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn build(name: &str, gtype: GraphType, f: impl FnOnce(&Graphz, &mut StringSerializer)) -> String {
        let mut ser = StringSerializer::new();
        let g = Graphz::new(name, gtype, &mut ser);
        f(&g, &mut ser);
        g.close(&mut ser);
        ser.into_string()
    }

    #[test]
    fn simple_graph() {
        let out = build("G", GraphType::Graph, |_, _| {});
        assert_eq!(out, "graph \"G\" {\n}");
    }

    #[test]
    fn simple_digraph() {
        let out = build("G", GraphType::DiGraph, |_, _| {});
        assert_eq!(out, "digraph \"G\" {\n}");
    }

    #[test]
    fn simple_graph_with_comment() {
        let mut ser = StringSerializer::new();
        let opts = GraphzOptions {
            comment: Some("this is comment".to_string()),
            ..Default::default()
        };
        let g = Graphz::apply("G", GraphType::Graph, &mut ser, &opts);
        g.close(&mut ser);
        assert_eq!(ser.into_string(), "// this is comment\ngraph \"G\" {\n}");
    }

    #[test]
    fn graph_two_nodes_one_edge() {
        let out = build("G", GraphType::Graph, |g, ser| {
            g.edge("Hello", "World", None, None, None, ser);
        });
        assert_eq!(out, "graph \"G\" {\n  \"Hello\" -- \"World\"\n}");
    }

    #[test]
    fn digraph_two_nodes_one_edge() {
        let out = build("G", GraphType::DiGraph, |g, ser| {
            g.edge("Hello", "World", None, None, None, ser);
        });
        assert_eq!(out, "digraph \"G\" {\n  \"Hello\" -> \"World\"\n}");
    }

    #[test]
    fn digraph_nodes_with_style() {
        let out = build("G", GraphType::DiGraph, |g, ser| {
            g.node("Hello", GraphShape::Box, None, None, None, None, None, ser);
            g.node("World", GraphShape::DoubleCircle, None, None, None, None, None, ser);
            g.edge("Hello", "World", None, None, None, ser);
        });
        assert_eq!(
            out,
            "digraph \"G\" {\n  \"Hello\" [shape=box]\n  \"World\" [shape=doublecircle]\n  \"Hello\" -> \"World\"\n}"
        );
    }

    #[test]
    fn digraph_with_simple_subgraphs() {
        fn process1(g: &Graphz, ser: &mut StringSerializer) {
            let sg = Graphz::subgraph("", GraphType::DiGraph, ser, None, None, None, None, None);
            sg.node("A", GraphShape::Circle, None, None, None, None, None, ser);
            sg.node("B", GraphShape::Circle, None, None, None, None, None, ser);
            sg.node("C", GraphShape::Circle, None, None, None, None, None, ser);
            sg.edge("A", "B", None, None, None, ser);
            sg.edge("B", "C", None, None, None, ser);
            sg.close(ser);
            let _ = g;
        }
        fn process2(g: &Graphz, ser: &mut StringSerializer) {
            let sg = Graphz::subgraph("", GraphType::DiGraph, ser, None, None, None, None, None);
            sg.node("K", GraphShape::Circle, None, None, None, None, None, ser);
            sg.node("L", GraphShape::Circle, None, None, None, None, None, ser);
            sg.node("M", GraphShape::Circle, None, None, None, None, None, ser);
            sg.edge("K", "L", None, None, None, ser);
            sg.edge("L", "M", None, None, None, ser);
            sg.close(ser);
            let _ = g;
        }

        let out = build("Process", GraphType::DiGraph, |g, ser| {
            g.node("0", GraphShape::Circle, None, None, None, None, None, ser);
            process1(g, ser);
            g.edge("0", "A", None, None, None, ser);
            process2(g, ser);
            g.edge("0", "K", None, None, None, ser);
            g.node("1", GraphShape::Circle, None, None, None, None, None, ser);
            g.edge("M", "1", None, None, None, ser);
            g.edge("C", "1", None, None, None, ser);
        });
        assert_eq!(
            out,
            "digraph \"Process\" {\n  \"0\"\n  subgraph {\n    \"A\"\n    \"B\"\n    \"C\"\n    \"A\" -> \"B\"\n    \"B\" -> \"C\"\n  }\n  \"0\" -> \"A\"\n  subgraph {\n    \"K\"\n    \"L\"\n    \"M\"\n    \"K\" -> \"L\"\n    \"L\" -> \"M\"\n  }\n  \"0\" -> \"K\"\n  \"1\"\n  \"M\" -> \"1\"\n  \"C\" -> \"1\"\n}"
        );
    }

    #[test]
    fn digraph_with_fancy_subgraphs() {
        fn process1(ser: &mut StringSerializer) {
            let sg = Graphz::subgraph(
                "cluster_p1",
                GraphType::DiGraph,
                ser,
                Some("process #1"),
                None,
                None,
                None,
                Some("blue"),
            );
            sg.node("A", GraphShape::Circle, None, None, None, None, None, ser);
            sg.node("B", GraphShape::Circle, None, None, None, None, None, ser);
            sg.node("C", GraphShape::Circle, None, None, None, None, None, ser);
            sg.edge("A", "B", None, None, None, ser);
            sg.edge("B", "C", None, None, None, ser);
            sg.close(ser);
        }
        fn process2(ser: &mut StringSerializer) {
            let sg = Graphz::subgraph(
                "cluster_p2",
                GraphType::DiGraph,
                ser,
                Some("process #2"),
                None,
                None,
                None,
                Some("green"),
            );
            sg.node("K", GraphShape::Circle, None, None, None, None, None, ser);
            sg.node("L", GraphShape::Circle, None, None, None, None, None, ser);
            sg.node("M", GraphShape::Circle, None, None, None, None, None, ser);
            sg.edge("K", "L", None, None, None, ser);
            sg.edge("L", "M", None, None, None, ser);
            sg.close(ser);
        }

        let out = build("Process", GraphType::DiGraph, |g, ser| {
            g.node("0", GraphShape::Circle, None, None, None, None, None, ser);
            process1(ser);
            g.edge("0", "A", None, None, None, ser);
            process2(ser);
            g.edge("0", "K", None, None, None, ser);
            g.node("1", GraphShape::Circle, None, None, None, None, None, ser);
            g.edge("M", "1", None, None, None, ser);
            g.edge("C", "1", None, None, None, ser);
        });
        assert_eq!(
            out,
            "digraph \"Process\" {\n  \"0\"\n  subgraph \"cluster_p1\" {\n    label = \"process #1\"\n    color=blue\n    \"A\"\n    \"B\"\n    \"C\"\n    \"A\" -> \"B\"\n    \"B\" -> \"C\"\n  }\n  \"0\" -> \"A\"\n  subgraph \"cluster_p2\" {\n    label = \"process #2\"\n    color=green\n    \"K\"\n    \"L\"\n    \"M\"\n    \"K\" -> \"L\"\n    \"L\" -> \"M\"\n  }\n  \"0\" -> \"K\"\n  \"1\"\n  \"M\" -> \"1\"\n  \"C\" -> \"1\"\n}"
        );
    }

    #[test]
    fn blockchain_simple() {
        fn lvl1(ser: &mut StringSerializer) {
            let sg = Graphz::subgraph("", GraphType::DiGraph, ser, None, Some(GraphRank::Same), None, None, None);
            sg.node("1", GraphShape::Circle, None, None, None, None, None, ser);
            sg.node("ddeecc", GraphShape::Box, None, None, None, None, None, ser);
            sg.node("ffeeff", GraphShape::Box, None, None, None, None, None, ser);
            sg.close(ser);
        }
        fn lvl0(ser: &mut StringSerializer) {
            let sg = Graphz::subgraph("", GraphType::DiGraph, ser, None, Some(GraphRank::Same), None, None, None);
            sg.node("0", GraphShape::Circle, None, None, None, None, None, ser);
            sg.node("000000", GraphShape::Box, None, None, None, None, None, ser);
            sg.close(ser);
        }
        fn timeline(ser: &mut StringSerializer) {
            let sg = Graphz::subgraph("timeline", GraphType::DiGraph, ser, None, None, None, None, None);
            sg.node("3", GraphShape::PlainText, None, None, None, None, None, ser);
            sg.node("2", GraphShape::PlainText, None, None, None, None, None, ser);
            sg.node("1", GraphShape::PlainText, None, None, None, None, None, ser);
            sg.node("0", GraphShape::PlainText, None, None, None, None, None, ser);
            sg.edge("0", "1", None, None, None, ser);
            sg.edge("1", "2", None, None, None, ser);
            sg.edge("2", "3", None, None, None, ser);
            sg.close(ser);
        }

        let mut ser = StringSerializer::new();
        let opts = GraphzOptions {
            rankdir: Some(GraphRankDir::BT),
            ..Default::default()
        };
        let g = Graphz::apply("Blockchain", GraphType::DiGraph, &mut ser, &opts);
        lvl1(&mut ser);
        g.edge("000000", "ffeeff", None, None, None, &mut ser);
        g.edge("000000", "ddeecc", None, None, None, &mut ser);
        lvl0(&mut ser);
        timeline(&mut ser);
        g.close(&mut ser);

        assert_eq!(
            ser.into_string(),
            "digraph \"Blockchain\" {\n  rankdir=BT\n  subgraph {\n    rank=same\n    \"1\"\n    \"ddeecc\" [shape=box]\n    \"ffeeff\" [shape=box]\n  }\n  \"000000\" -> \"ffeeff\"\n  \"000000\" -> \"ddeecc\"\n  subgraph {\n    rank=same\n    \"0\"\n    \"000000\" [shape=box]\n  }\n  subgraph \"timeline\" {\n    \"3\" [shape=plaintext]\n    \"2\" [shape=plaintext]\n    \"1\" [shape=plaintext]\n    \"0\" [shape=plaintext]\n    \"0\" -> \"1\"\n    \"1\" -> \"2\"\n    \"2\" -> \"3\"\n  }\n}"
        );
    }

    #[test]
    fn process_example() {
        let out = build("G", GraphType::Graph, |g, ser| {
            g.edge("run", "intr", None, None, None, ser);
            g.edge("intr", "runbl", None, None, None, ser);
            g.edge("runbl", "run", None, None, None, ser);
            g.edge("run", "kernel", None, None, None, ser);
            g.edge("kernel", "zombie", None, None, None, ser);
            g.edge("kernel", "sleep", None, None, None, ser);
            g.edge("kernel", "runmem", None, None, None, ser);
            g.edge("sleep", "swap", None, None, None, ser);
            g.edge("swap", "runswap", None, None, None, ser);
            g.edge("runswap", "new", None, None, None, ser);
            g.edge("runswap", "runmem", None, None, None, ser);
            g.edge("new", "runmem", None, None, None, ser);
            g.edge("sleep", "runmem", None, None, None, ser);
        });
        assert_eq!(
            out,
            "graph \"G\" {\n  \"run\" -- \"intr\"\n  \"intr\" -- \"runbl\"\n  \"runbl\" -- \"run\"\n  \"run\" -- \"kernel\"\n  \"kernel\" -- \"zombie\"\n  \"kernel\" -- \"sleep\"\n  \"kernel\" -- \"runmem\"\n  \"sleep\" -- \"swap\"\n  \"swap\" -- \"runswap\"\n  \"runswap\" -- \"new\"\n  \"runswap\" -- \"runmem\"\n  \"new\" -- \"runmem\"\n  \"sleep\" -- \"runmem\"\n}"
        );
    }

    #[test]
    fn huge_graph_is_stack_safe() {
        let mut ser = StringSerializer::new();
        let g = Graphz::new("G", GraphType::DiGraph, &mut ser);
        for i in 1..=1000 {
            g.edge(&format!("e{i}"), &format!("e{}", i + 1), None, None, None, &mut ser);
        }
        g.close(&mut ser);
        let out = ser.into_string();
        assert_eq!(out.matches(" -> ").count(), 1000);
    }
}
