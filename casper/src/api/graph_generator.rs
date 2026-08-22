//! Graphviz DAG rendering (port of `api/GraphGenerator.scala`).
//!
//! Builds a DOT cluster-per-validator view of the DAG using the synchronous `rchain_graphz`
//! builder. The Scala `showJustificationLines` is hardcoded `true`, so the justification dotted
//! lines are never drawn (kept here, `#[allow(dead_code)]`, for fidelity).

use std::collections::{BTreeMap, BTreeSet};

use rchain_graphz::{
    GraphArrowType, GraphRankDir, GraphSerializer, GraphShape, GraphStyle, GraphType, Graphz,
    GraphzOptions,
};

/// A block in the DAG view (port of `ValidatorBlock`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ValidatorBlock {
    pub id: String,
    pub sender: String,
    pub height: i64,
    pub justifications: Vec<String>,
    pub fringe: BTreeSet<String>,
}

/// Blocks grouped by height for a single validator (port of `ValidatorsBlocks`).
type ValidatorsBlocks = BTreeMap<i64, Vec<ValidatorBlock>>;

/// The accumulated DAG info (port of `DagInfo`).
#[derive(Clone, Debug)]
struct DagInfo {
    validators: BTreeMap<String, ValidatorsBlocks>,
    timeseries: BTreeSet<i64>,
}

impl DagInfo {
    fn empty() -> Self {
        DagInfo {
            validators: BTreeMap::new(),
            timeseries: BTreeSet::new(),
        }
    }
}

/// Render the DAG as a cluster-per-validator Graphviz graph (port of `dagAsCluster`).
pub fn dag_as_cluster<S: GraphSerializer>(blocks: &[ValidatorBlock], ser: &mut S) {
    let mut acc = DagInfo::empty();
    for b in blocks {
        accumulate_dag_info(&mut acc, b);
    }
    let block_color_map = generate_fringe_color_mapping(blocks);
    let timeseries: Vec<i64> = acc.timeseries.iter().copied().collect();
    let lowest_height = timeseries[0];
    let validators_list: Vec<(String, ValidatorsBlocks)> = acc.validators.into_iter().collect();

    let g = init_graph("dag", ser);

    let mut all_ancestors: Vec<String> = validators_list
        .iter()
        .flat_map(|(_, blocks)| {
            blocks
                .get(&lowest_height)
                .map(|bs| {
                    bs.iter()
                        .flat_map(|b| b.justifications.clone())
                        .collect::<Vec<String>>()
                })
                .unwrap_or_default()
        })
        .collect();
    all_ancestors.sort();
    all_ancestors.dedup();

    // Invisible edges from ancestors to the first node of each cluster (for alignment).
    for (val_id, blocks) in &validators_list {
        for ancestor in &all_ancestors {
            for node in nodes_for_height(lowest_height, blocks, val_id, &block_color_map).keys() {
                g.edge(ancestor, node, Some(GraphStyle::Invis), None, None, ser);
            }
        }
    }

    // One cluster per validator.
    for (id, blocks) in &validators_list {
        validator_cluster(id, blocks, &timeseries, &block_color_map, ser);
    }

    // Parent (justification) dependencies.
    let validator_blocks: Vec<ValidatorsBlocks> =
        validators_list.iter().map(|(_, b)| b.clone()).collect();
    draw_parent_dependencies(&g, &validator_blocks, ser);

    // Justification dotted lines are disabled (the Scala hardcodes `showJustificationLines = true`).
    g.close(ser);
}

fn accumulate_dag_info(acc: &mut DagInfo, block: &ValidatorBlock) {
    acc.timeseries.insert(block.height);
    let validator_blocks = acc.validators.entry(block.sender.clone()).or_default();
    validator_blocks
        .entry(block.height)
        .or_default()
        .push(block.clone());
}

fn init_graph<S: GraphSerializer>(name: &str, ser: &mut S) -> Graphz {
    let opts = GraphzOptions {
        rankdir: Some(GraphRankDir::BT),
        splines: Some("false".to_string()),
        graph: vec![("fontsize".to_string(), "12".to_string())],
        node: vec![
            ("width".to_string(), "0".to_string()),
            ("height".to_string(), "0".to_string()),
            ("margin".to_string(), "\".1,.05\"".to_string()),
            ("fontsize".to_string(), "12".to_string()),
        ],
        edge: vec![
            ("arrowsize".to_string(), ".5".to_string()),
            ("arrowhead".to_string(), "open".to_string()),
            ("penwidth".to_string(), ".6".to_string()),
        ],
        ..Default::default()
    };
    Graphz::apply(name, GraphType::DiGraph, ser, &opts)
}

fn validator_cluster<S: GraphSerializer>(
    validator_id: &str,
    blocks: &ValidatorsBlocks,
    timeseries: &[i64],
    block_color_map: &BTreeMap<String, (Option<String>, Option<String>)>,
    ser: &mut S,
) {
    let g = Graphz::subgraph(
        &format!("cluster_{validator_id}"),
        GraphType::DiGraph,
        ser,
        Some(validator_id),
        None,
        None,
        None,
        None,
    );
    let nodes: Vec<BTreeMap<String, (Option<GraphStyle>, Option<String>, Option<String>)>> =
        timeseries
            .iter()
            .map(|ts| nodes_for_height(*ts, blocks, validator_id, block_color_map))
            .collect();
    for ns in &nodes {
        for (name, (style, fill, border)) in ns {
            let border_width = border.as_ref().map(|_| 2);
            let border_default = border.clone().or_else(|| Some("#828282".to_string()));
            g.node(
                name,
                GraphShape::DoubleOctagon,
                *style,
                fill.as_deref(),
                border_default.as_deref(),
                border_width,
                None,
                ser,
            );
        }
    }
    for pair in nodes.windows(2) {
        for n1 in pair[0].keys() {
            for n2 in pair[1].keys() {
                g.edge(n1, n2, Some(GraphStyle::Invis), None, None, ser);
            }
        }
    }
    g.close(ser);
}

fn draw_parent_dependencies<S: GraphSerializer>(
    g: &Graphz,
    validators: &[ValidatorsBlocks],
    ser: &mut S,
) {
    for blocks in validators {
        for bs in blocks.values() {
            for b in bs {
                for p in &b.justifications {
                    g.edge(&b.id, p, None, None, Some(false), ser);
                }
            }
        }
    }
}

#[allow(dead_code)]
fn draw_justification_dotted_lines<S: GraphSerializer>(
    g: &Graphz,
    validators: &BTreeMap<String, ValidatorsBlocks>,
    ser: &mut S,
) {
    for blocks in validators.values() {
        for bs in blocks.values() {
            for b in bs {
                for j in &b.justifications {
                    g.edge(
                        &b.id,
                        j,
                        Some(GraphStyle::Dotted),
                        Some(GraphArrowType::NoneArrow),
                        Some(false),
                        ser,
                    );
                }
            }
        }
    }
}

fn generate_fringe_color_mapping(
    blocks: &[ValidatorBlock],
) -> BTreeMap<String, (Option<String>, Option<String>)> {
    const COLORS: [&str; 7] = [
        "#ff5e5e", "#b561ff", "#00b803", "#636eff", "#8dff87", "#00d9ff", "#ffc400",
    ];
    let block_map: BTreeMap<&str, &ValidatorBlock> =
        blocks.iter().map(|b| (b.id.as_str(), b)).collect();

    // Group blocks by their (non-empty) fringe, keyed by the fringe's block ids.
    let mut groups: BTreeMap<BTreeSet<String>, BTreeSet<String>> = BTreeMap::new();
    for b in blocks {
        let fringe: BTreeSet<String> = b.fringe.iter().cloned().collect();
        if !fringe.is_empty() {
            groups.entry(fringe).or_default().insert(b.id.clone());
        }
    }

    // Sort by the max height among the fringe's blocks.
    let mut ordered: Vec<(BTreeSet<String>, BTreeSet<String>)> = groups.into_iter().collect();
    ordered.sort_by_key(|(fringe, _)| {
        fringe
            .iter()
            .filter_map(|id| block_map.get(id.as_str()))
            .map(|b| b.height)
            .max()
            .unwrap_or(-1)
    });

    let mut fill_map: BTreeMap<String, String> = BTreeMap::new();
    let mut border_map: BTreeMap<String, String> = BTreeMap::new();
    for ((fringe, seen), color) in ordered.iter().zip(COLORS.iter().cycle()) {
        for id in fringe {
            fill_map.insert(id.clone(), (*color).to_string());
        }
        for id in seen {
            border_map.insert(id.clone(), (*color).to_string());
        }
    }

    let mut result = BTreeMap::new();
    for id in fill_map.keys().chain(border_map.keys()) {
        result.insert(
            id.clone(),
            (fill_map.get(id).cloned(), border_map.get(id).cloned()),
        );
    }
    result
}

fn nodes_for_height(
    height: i64,
    blocks: &ValidatorsBlocks,
    validator_id: &str,
    block_color_map: &BTreeMap<String, (Option<String>, Option<String>)>,
) -> BTreeMap<String, (Option<GraphStyle>, Option<String>, Option<String>)> {
    match blocks.get(&height) {
        Some(bs) => bs
            .iter()
            .map(|b| (b.id.clone(), style_for_node(&b.id, block_color_map)))
            .collect(),
        None => {
            let mut m = BTreeMap::new();
            m.insert(
                format!("{}_{}", height, validator_id),
                (Some(GraphStyle::Invis), None, None),
            );
            m
        }
    }
}

fn style_for_node(
    block_id: &str,
    block_color_map: &BTreeMap<String, (Option<String>, Option<String>)>,
) -> (Option<GraphStyle>, Option<String>, Option<String>) {
    match block_color_map.get(block_id) {
        Some((fill, border)) => (
            fill.as_ref().map(|_| GraphStyle::Filled),
            fill.clone(),
            border.clone(),
        ),
        None => (None, None, None),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rchain_graphz::StringSerializer;

    fn block(
        id: &str,
        sender: &str,
        height: i64,
        justifications: Vec<&str>,
        fringe: Vec<&str>,
    ) -> ValidatorBlock {
        ValidatorBlock {
            id: id.to_string(),
            sender: sender.to_string(),
            height,
            justifications: justifications.into_iter().map(String::from).collect(),
            fringe: fringe.into_iter().map(String::from).collect(),
        }
    }

    #[test]
    fn renders_dag_cluster() {
        let blocks = vec![
            block("aaaa1", "val1", 0, vec![], vec![]),
            block("bbbb2", "val1", 1, vec!["aaaa1"], vec![]),
        ];
        let mut ser = StringSerializer::new();
        dag_as_cluster(&blocks, &mut ser);
        let out = ser.into_string();

        assert!(out.contains("digraph \"dag\""));
        assert!(out.contains("cluster_val1"));
        assert!(out.contains("doubleoctagon"));
        assert!(out.contains("\"bbbb2\" -> \"aaaa1\""));
    }
}
