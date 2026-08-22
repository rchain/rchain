//! Sequential trie export (port of `RadixTree.sequentialExport`).
//!
//! Walks the radix trie from `root_hash`, resuming at `last_prefix` if provided, and emits up to
//! `take_size` nodes/leaves after skipping `skip_size`. The `ExportDataSettings` flags control
//! which of the five streams are collected.

use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;

use crate::history::key_segment::KeySegment;
use crate::history::radix_tree::{decode, Item, Node, SerializedNode};

/// The exported node/leaf streams (port of `RadixTree.ExportData`).
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ExportData {
    pub node_prefixes: Vec<KeySegment>,
    pub node_keys: Vec<Blake2b256Hash>,
    pub node_values: Vec<Vec<u8>>,
    pub leaf_prefixes: Vec<KeySegment>,
    pub leaf_values: Vec<Blake2b256Hash>,
}

/// Which `ExportData` streams to collect (port of `RadixTree.ExportDataSettings`).
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ExportDataSettings {
    pub flag_node_prefixes: bool,
    pub flag_node_keys: bool,
    pub flag_node_values: bool,
    pub flag_leaf_prefixes: bool,
    pub flag_leaf_values: bool,
}

/// The `getNodeDataFromStore` callback: reads the serialized bytes of a node, or `None` if absent.
pub type NodeDataReader<'a> = &'a dyn Fn(&Blake2b256Hash) -> Option<Vec<u8>>;

#[derive(Clone)]
struct NodeData {
    prefix: KeySegment,
    decoded: Node,
    last_item_index: Option<u8>,
}

struct StepData {
    path: Vec<NodeData>,
    skip: i32,
    take: i32,
    exp_data: ExportData,
}

fn err_not_found(prefix: &KeySegment) -> String {
    format!(
        "Export error: node with prefix {} not found.",
        prefix.to_hex()
    )
}

/// Build the path from `root_hash` down to the node at `last_prefix` (port of `initNodePath`).
fn init_node_path(
    root_hash: Blake2b256Hash,
    last_prefix: &KeySegment,
    get_node_data: NodeDataReader,
) -> Result<Vec<NodeData>, String> {
    let mut hash = root_hash;
    let mut node_prefix = KeySegment::empty();
    let mut rest_prefix = last_prefix.clone();
    let mut path: Vec<NodeData> = Vec::new();

    loop {
        let node_bytes = get_node_data(&hash)
            .ok_or_else(|| format!("Export error: node with key {} not found.", hash.to_hex()))?;
        let node = decode(&SerializedNode::try_from(node_bytes.as_slice())?);
        if rest_prefix.is_empty() {
            path.insert(
                0,
                NodeData {
                    prefix: node_prefix,
                    decoded: node,
                    last_item_index: None,
                },
            );
            return Ok(path);
        }
        let item_idx = rest_prefix.head() as usize;
        match node[item_idx].clone() {
            Item::NodePtr { prefix: ptr_prefix, ptr } => {
                let (prefix_common, prefix_rest, ptr_prefix_rest) =
                    KeySegment::common_prefix(&rest_prefix.tail(), &ptr_prefix);
                assert!(
                    ptr_prefix_rest.is_empty(),
                    "Export error: node with prefix {} not found.",
                    node_prefix.concat(&rest_prefix).to_hex()
                );
                path.insert(
                    0,
                    NodeData {
                        prefix: node_prefix.clone(),
                        decoded: node,
                        last_item_index: Some(rest_prefix.head()),
                    },
                );
                hash = ptr;
                node_prefix = node_prefix.append(rest_prefix.head()).concat(&prefix_common);
                rest_prefix = prefix_rest;
            }
            _ => return Err(err_not_found(&node_prefix.concat(&rest_prefix))),
        }
    }
}

/// Find the next non-empty item at or after `last_idx` (port of `findNextNonEmptyItem`).
fn find_next_non_empty_item(
    node: &Node,
    last_idx: Option<u8>,
    settings: &ExportDataSettings,
) -> Option<(u8, Item)> {
    if last_idx == Some(0xFF) {
        return None;
    }
    let cur_idx_int = match last_idx {
        Some(idx) => idx as usize + 1,
        None => 0,
    };
    let cur_item = node[cur_idx_int].clone();
    let cur_idx = cur_idx_int as u8;
    match &cur_item {
        Item::Empty => find_next_non_empty_item(node, Some(cur_idx), settings),
        Item::Leaf { .. } => {
            if settings.flag_leaf_prefixes || settings.flag_leaf_values {
                Some((cur_idx, cur_item))
            } else {
                find_next_non_empty_item(node, Some(cur_idx), settings)
            }
        }
        Item::NodePtr { .. } => Some((cur_idx, cur_item)),
    }
}

fn add_leaf(
    p: &StepData,
    leaf_prefix: &KeySegment,
    leaf_hash: Blake2b256Hash,
    item_index: u8,
    cur_node_prefix: &KeySegment,
    new_path: Vec<NodeData>,
    settings: &ExportDataSettings,
) -> StepData {
    if p.skip > 0 {
        return StepData {
            path: new_path,
            skip: p.skip,
            take: p.take,
            exp_data: p.exp_data.clone(),
        };
    }
    let mut leaf_prefixes = p.exp_data.leaf_prefixes.clone();
    let mut leaf_values = p.exp_data.leaf_values.clone();
    if settings.flag_leaf_prefixes {
        leaf_prefixes.push(cur_node_prefix.append(item_index).concat(leaf_prefix));
    }
    if settings.flag_leaf_values {
        leaf_values.push(leaf_hash);
    }
    StepData {
        path: new_path,
        skip: p.skip,
        take: p.take,
        exp_data: ExportData {
            node_prefixes: p.exp_data.node_prefixes.clone(),
            node_keys: p.exp_data.node_keys.clone(),
            node_values: p.exp_data.node_values.clone(),
            leaf_prefixes,
            leaf_values,
        },
    }
}

fn add_node_ptr(
    p: &StepData,
    ptr_prefix: &KeySegment,
    ptr: Blake2b256Hash,
    item_index: u8,
    cur_node_prefix: &KeySegment,
    new_path: Vec<NodeData>,
    get_node_data: NodeDataReader,
    settings: &ExportDataSettings,
) -> Result<StepData, String> {
    let child_bytes = get_node_data(&ptr)
        .ok_or_else(|| format!("Export error: Node with key {} not found", ptr.to_hex()))?;
    let child_decoded = decode(&SerializedNode::try_from(child_bytes.as_slice())?);
    let child_np = cur_node_prefix.append(item_index).concat(ptr_prefix);
    let child_node_data = NodeData {
        prefix: child_np.clone(),
        decoded: child_decoded,
        last_item_index: None,
    };
    let mut child_path = new_path;
    child_path.insert(0, child_node_data);

    if p.skip > 0 {
        return Ok(StepData {
            path: child_path,
            skip: p.skip - 1,
            take: p.take,
            exp_data: p.exp_data.clone(),
        });
    }

    let mut node_prefixes = p.exp_data.node_prefixes.clone();
    let mut node_keys = p.exp_data.node_keys.clone();
    let mut node_values = p.exp_data.node_values.clone();
    if settings.flag_node_prefixes {
        node_prefixes.push(child_np);
    }
    if settings.flag_node_keys {
        node_keys.push(ptr);
    }
    if settings.flag_node_values {
        node_values.push(child_bytes);
    }
    Ok(StepData {
        path: child_path,
        skip: p.skip,
        take: p.take - 1,
        exp_data: ExportData {
            node_prefixes,
            node_keys,
            node_values,
            leaf_prefixes: p.exp_data.leaf_prefixes.clone(),
            leaf_values: p.exp_data.leaf_values.clone(),
        },
    })
}

fn add_element(
    p: &StepData,
    item_index: u8,
    item: &Item,
    cur_node: &Node,
    cur_node_prefix: &KeySegment,
    get_node_data: NodeDataReader,
    settings: &ExportDataSettings,
) -> Result<StepData, String> {
    let new_cur_node_data = NodeData {
        prefix: cur_node_prefix.clone(),
        decoded: cur_node.clone(),
        last_item_index: Some(item_index),
    };
    let mut new_path = Vec::with_capacity(p.path.len());
    new_path.push(new_cur_node_data);
    new_path.extend_from_slice(&p.path[1..]);

    match item {
        Item::Empty => Ok(StepData {
            path: new_path,
            skip: p.skip,
            take: p.take,
            exp_data: p.exp_data.clone(),
        }),
        Item::Leaf { prefix, value } => Ok(add_leaf(
            p,
            prefix,
            *value,
            item_index,
            cur_node_prefix,
            new_path,
            settings,
        )),
        Item::NodePtr { prefix, ptr } => add_node_ptr(
            p,
            prefix,
            *ptr,
            item_index,
            cur_node_prefix,
            new_path,
            get_node_data,
            settings,
        ),
    }
}

enum ExportStep {
    Continue(StepData),
    Done(ExportData, Option<KeySegment>),
}

fn export_step(
    p: StepData,
    get_node_data: NodeDataReader,
    settings: &ExportDataSettings,
) -> Result<ExportStep, String> {
    if p.path.is_empty() {
        return Ok(ExportStep::Done(p.exp_data, None));
    }
    let cur_node_data = &p.path[0];
    let cur_node_prefix = cur_node_data.prefix.clone();
    let cur_node = cur_node_data.decoded.clone();
    if p.skip == 0 && p.take == 0 {
        return Ok(ExportStep::Done(p.exp_data, Some(cur_node_prefix)));
    }
    match find_next_non_empty_item(&cur_node, cur_node_data.last_item_index, settings) {
        Some((item_index, item)) => {
            let next = add_element(
                &p,
                item_index,
                &item,
                &cur_node,
                &cur_node_prefix,
                get_node_data,
                settings,
            )?;
            Ok(ExportStep::Continue(next))
        }
        None => Ok(ExportStep::Continue(StepData {
            path: p.path[1..].to_vec(),
            skip: p.skip,
            take: p.take,
            exp_data: p.exp_data,
        })),
    }
}

/// Walk the trie and export nodes/leaves (port of `RadixTree.sequentialExport`).
pub fn sequential_export(
    root_hash: Blake2b256Hash,
    last_prefix: Option<KeySegment>,
    skip_size: i32,
    take_size: i32,
    get_node_data: NodeDataReader,
    settings: &ExportDataSettings,
) -> Result<(ExportData, Option<KeySegment>), String> {
    if skip_size == 0 && take_size == 0 {
        return Err(
            "Export error: invalid initial conditions (skipSize, takeSize)==(0,0).".to_string(),
        );
    }
    let root_node_ser = match get_node_data(&root_hash) {
        Some(bytes) => bytes,
        None => return Ok((ExportData::default(), None)),
    };

    let empty_export_data = ExportData::default();
    let no_root_start = (empty_export_data.clone(), skip_size, take_size);
    let skipped_start = (empty_export_data.clone(), skip_size - 1, take_size);

    let root_export_data = {
        let mut node_prefixes = Vec::new();
        let mut node_keys = Vec::new();
        let mut node_values = Vec::new();
        if settings.flag_node_prefixes {
            node_prefixes.push(KeySegment::empty());
        }
        if settings.flag_node_keys {
            node_keys.push(root_hash);
        }
        if settings.flag_node_values {
            node_values.push(root_node_ser);
        }
        ExportData {
            node_prefixes,
            node_keys,
            node_values,
            leaf_prefixes: Vec::new(),
            leaf_values: Vec::new(),
        }
    };
    let root_start = (root_export_data, skip_size, take_size - 1);

    let (init_export_data, init_skip_size, init_take_size) = if last_prefix.is_some() {
        no_root_start
    } else if skip_size > 0 {
        skipped_start
    } else {
        root_start
    };

    let path = init_node_path(
        root_hash,
        &last_prefix.unwrap_or_else(KeySegment::empty),
        get_node_data,
    )?;
    let mut step = StepData {
        path,
        skip: init_skip_size,
        take: init_take_size,
        exp_data: init_export_data,
    };
    loop {
        match export_step(step, get_node_data, settings)? {
            ExportStep::Continue(next) => step = next,
            ExportStep::Done(data, prefix) => return Ok((data, prefix)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    use crate::history::radix_tree::{empty_node, empty_root_hash, hash_node};

    fn all_settings() -> ExportDataSettings {
        ExportDataSettings {
            flag_node_prefixes: true,
            flag_node_keys: true,
            flag_node_values: true,
            flag_leaf_prefixes: true,
            flag_leaf_values: true,
        }
    }

    #[test]
    fn export_empty_root() {
        let root_hash = empty_root_hash();
        let store: HashMap<Blake2b256Hash, Vec<u8>> =
            HashMap::from([(root_hash, Vec::new())]);
        let get = |h: &Blake2b256Hash| store.get(h).cloned();

        let (data, last_prefix) =
            sequential_export(root_hash, None, 0, 10, &get, &all_settings()).unwrap();
        assert_eq!(data.node_keys, vec![root_hash]);
        assert!(data.leaf_values.is_empty());
        assert_eq!(last_prefix, None);
    }

    #[test]
    fn export_single_leaf() {
        let mut root = empty_node();
        let leaf_hash = Blake2b256Hash::from_bytes([0x42; 32]);
        root[0] = Item::Leaf {
            prefix: KeySegment::new(vec![1]),
            value: leaf_hash,
        };
        let (root_hash, root_bytes) = hash_node(&root);
        let store: HashMap<Blake2b256Hash, Vec<u8>> = HashMap::from([(root_hash, root_bytes)]);

        let (data, last_prefix) =
            sequential_export(root_hash, None, 0, 10, &|h| store.get(h).cloned(), &all_settings())
                .unwrap();

        assert_eq!(data.node_keys, vec![root_hash]);
        assert_eq!(data.leaf_values, vec![leaf_hash]);
        assert_eq!(data.leaf_prefixes, vec![KeySegment::new(vec![0, 1])]);
        assert_eq!(last_prefix, None);
    }
}
