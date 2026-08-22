//! Content-addressed radix trie (Law 10: Merkle determinism).
//!
//! Mirrors `rspace/src/main/scala/coop/rchain/rspace/history/RadixTree.scala`.

use std::collections::HashMap;
use std::sync::Arc;

use async_recursion::async_recursion;
use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;
use rchain_shared::typed_store::KeyValueTypedStore;

use crate::history::history_action::HistoryAction;
use crate::history::key_segment::KeySegment;

/// The number of child slots in a node (one per byte value).
pub const NUM_ITEMS: usize = 256;

/// A node slot (port of the sealed `Item` hierarchy).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Item {
    Empty,
    Leaf {
        prefix: KeySegment,
        value: Blake2b256Hash,
    },
    NodePtr {
        prefix: KeySegment,
        ptr: Blake2b256Hash,
    },
}

/// A node is a fixed 256-slot array of items (port of `RadixTree.Node`). The "exactly 256 slots"
/// invariant is carried structurally by the array type, so a short/corrupt node cannot be produced
/// by [`decode`] (which is total on the validated [`SerializedNode`] refinement).
pub type Node = [Item; NUM_ITEMS];

/// An empty node (port of `RadixTree.emptyNode`).
pub fn empty_node() -> Node {
    std::array::from_fn(|_| Item::Empty)
}

/// The hash of the empty node, i.e. the empty root (port of `RadixTree.emptyRootHash`).
pub fn empty_root_hash() -> Blake2b256Hash {
    hash_node(&empty_node()).0
}

/// Serialize a node to bytes (port of `RadixTree.Codecs.encode`).
pub fn encode(node: &Node) -> Vec<u8> {
    let mut size = 0;
    for item in node {
        match item {
            Item::Empty => {}
            Item::Leaf { prefix, .. } | Item::NodePtr { prefix, .. } => {
                size += 2 + prefix.len() + 32;
            }
        }
    }
    let mut out = Vec::with_capacity(size);
    for (idx, item) in node.iter().enumerate() {
        match item {
            Item::Empty => {}
            Item::Leaf { prefix, value } => {
                out.push(idx as u8);
                out.push((prefix.len() & 0x7F) as u8);
                out.extend_from_slice(prefix.as_bytes());
                out.extend_from_slice(value.as_bytes());
            }
            Item::NodePtr { prefix, ptr } => {
                out.push(idx as u8);
                out.push((0x80 | (prefix.len() & 0x7F)) as u8);
                out.extend_from_slice(prefix.as_bytes());
                out.extend_from_slice(ptr.as_bytes());
            }
        }
    }
    out
}

/// A well-formed serialized radix node. `TryFrom<&[u8]>` validates the record framing (bounds,
/// prefix sizes, 32-byte values, and no duplicate item index); [`decode`] is then *total* on this
/// refinement — it cannot observe malformed bytes.
pub struct SerializedNode<'a>(&'a [u8]);

impl<'a> TryFrom<&'a [u8]> for SerializedNode<'a> {
    type Error = String;

    fn try_from(bytes: &'a [u8]) -> Result<Self, String> {
        let mut pos = 0;
        let mut seen = [false; 256];
        while pos < bytes.len() {
            if pos + 2 > bytes.len() {
                return Err("truncated radix node header".to_string());
            }
            let idx = bytes[pos] as usize;
            if seen[idx] {
                return Err(format!("duplicate item index {idx}"));
            }
            seen[idx] = true;
            let prefix_size = (bytes[pos + 1] & 0x7F) as usize;
            let prefix_start = pos + 2;
            if prefix_start + prefix_size > bytes.len() {
                return Err("truncated radix node prefix".to_string());
            }
            let val_start = prefix_start + prefix_size;
            if val_start + 32 > bytes.len() {
                return Err("truncated radix node value".to_string());
            }
            pos = val_start + 32;
        }
        Ok(SerializedNode(bytes))
    }
}

/// Deserialize a node from validated bytes (total; port of `RadixTree.Codecs.decode`).
pub fn decode(node: &SerializedNode) -> Node {
    let bytes = node.0;
    let mut result = empty_node();
    let mut pos = 0;
    while pos < bytes.len() {
        let idx = bytes[pos] as usize;
        let second = bytes[pos + 1];
        let prefix_size = (second & 0x7F) as usize;
        let prefix_start = pos + 2;
        let prefix = KeySegment::new(bytes[prefix_start..prefix_start + prefix_size].to_vec());
        let val_start = prefix_start + prefix_size;
        let val = Blake2b256Hash::from_byte_array(&bytes[val_start..val_start + 32]);
        let item = if (second & 0x80) == 0 {
            Item::Leaf { prefix, value: val }
        } else {
            Item::NodePtr { prefix, ptr: val }
        };
        result[idx] = item;
        pos = val_start + 32;
    }
    result
}

/// Hash a serialized node (port of `RadixTree.hashNode`).
pub fn hash_node(node: &Node) -> (Blake2b256Hash, Vec<u8>) {
    let bytes = encode(node);
    (Blake2b256Hash::create(&bytes), bytes)
}

/// The store-backed radix tree (port of `RadixTree.RadixTreeImpl`).
pub struct RadixTreeImpl {
    store: Arc<dyn KeyValueTypedStore<Blake2b256Hash, Vec<u8>>>,
    cache_read: std::sync::Mutex<HashMap<Blake2b256Hash, Node>>,
    cache_write: std::sync::Mutex<HashMap<Blake2b256Hash, Vec<u8>>>,
}

impl RadixTreeImpl {
    pub fn new(store: Arc<dyn KeyValueTypedStore<Blake2b256Hash, Vec<u8>>>) -> Self {
        RadixTreeImpl {
            store,
            cache_read: std::sync::Mutex::new(HashMap::new()),
            cache_write: std::sync::Mutex::new(HashMap::new()),
        }
    }

    async fn load_node_from_store(&self, node_ptr: Blake2b256Hash) -> Result<Option<Node>, String> {
        // `BytesCodec` cannot fail to decode, so the store error is unreachable.
        let bytes = self
            .store
            .get(&[node_ptr])
            .await
            .ok()
            .and_then(|v| v.into_iter().next().flatten());
        match bytes {
            None => Ok(None),
            Some(bytes) => {
                let serialized = SerializedNode::try_from(bytes.as_slice())
                    .map_err(|e| format!("corrupt node {}: {e}", node_ptr.to_hex()))?;
                Ok(Some(decode(&serialized)))
            }
        }
    }

    /// Load a node, using the read cache and falling back to the store (port of `loadNode`).
    pub async fn load_node(&self, node_ptr: Blake2b256Hash, no_assert: bool) -> Node {
        if let Some(node) = crate::lock::mlock(&self.cache_read).get(&node_ptr).cloned() {
            return node;
        }
        match self.load_node_from_store(node_ptr).await {
            Ok(Some(node)) => {
                crate::lock::mlock(&self.cache_read).insert(node_ptr, node.clone());
                node
            }
            Ok(None) => {
                assert!(
                    no_assert,
                    "Missing node in database. ptr={}",
                    node_ptr.to_hex()
                );
                empty_node()
            }
            Err(e) => {
                assert!(no_assert, "Corrupt node {}: {e}", node_ptr.to_hex());
                empty_node()
            }
        }
    }

    pub fn clear_read_cache(&self) {
        crate::lock::mlock(&self.cache_read).clear();
    }

    /// Serialize + hash a node, caching the bytes for a later commit (port of `saveNode`).
    pub fn save_node(&self, node: &Node) -> Blake2b256Hash {
        let (hash, bytes) = hash_node(node);
        let mut cache_read = crate::lock::mlock(&self.cache_read);
        if let Some(existing) = cache_read.get(&hash) {
            assert!(
                existing == node,
                "Collision in cache: record with key = {} has already existed.",
                hash.to_hex()
            );
        } else {
            cache_read.insert(hash, node.clone());
        }
        drop(cache_read);
        crate::lock::mlock(&self.cache_write).insert(hash, bytes);
        hash
    }

    /// Write the write-cache to the store, checking for collisions (port of `commit`).
    pub async fn commit(&self) -> Result<(), String> {
        let kv_pairs: Vec<(Blake2b256Hash, Vec<u8>)> = crate::lock::mlock(&self.cache_write)
            .iter()
            .map(|(k, v)| (*k, v.clone()))
            .collect();
        let keys: Vec<Blake2b256Hash> = kv_pairs.iter().map(|(k, _)| *k).collect();
        let present = self.store.contains(&keys).await?;
        let absent: Vec<(Blake2b256Hash, Vec<u8>)> = kv_pairs
            .iter()
            .zip(present.iter())
            .filter(|(_, &p)| !p)
            .map(|((k, v), _)| (*k, v.clone()))
            .collect();
        let existing_keys: Vec<Blake2b256Hash> = kv_pairs
            .iter()
            .zip(present.iter())
            .filter(|(_, &p)| p)
            .map(|((k, _), _)| *k)
            .collect();
        let existing_values = self.store.get(&existing_keys).await?;
        let cache_map: HashMap<Blake2b256Hash, Vec<u8>> = kv_pairs.iter().cloned().collect();
        for (k, existing) in existing_keys.iter().zip(existing_values.iter()) {
            let cached = cache_map
                .get(k)
                .ok_or_else(|| "cached key must be present".to_string())?;
            let stored = existing.clone().unwrap_or_default();
            if cached != &stored {
                return Err(format!("collision in KVDB (key = {})", k.to_hex()));
            }
        }
        self.store.put(&absent).await?;
        Ok(())
    }

    pub fn clear_write_cache(&self) {
        crate::lock::mlock(&self.cache_write).clear();
    }

    /// Read the leaf value at `start_prefix` under `start_node` (port of `read`).
    pub async fn read(
        &self,
        start_node: &Node,
        start_prefix: &KeySegment,
    ) -> Option<Blake2b256Hash> {
        let mut node = start_node.clone();
        let mut prefix = start_prefix.clone();
        loop {
            if prefix.is_empty() {
                return None;
            }
            match &node[prefix.head() as usize] {
                Item::Empty => return None,
                Item::Leaf {
                    prefix: leaf_prefix,
                    value,
                } => {
                    return if *leaf_prefix == prefix.tail() {
                        Some(*value)
                    } else {
                        None
                    };
                }
                Item::NodePtr {
                    prefix: ptr_prefix,
                    ptr,
                } => {
                    let (_, prefix_rest, ptr_prefix_rest) =
                        KeySegment::common_prefix(&prefix.tail(), ptr_prefix);
                    if ptr_prefix_rest.is_empty() {
                        node = self.load_node(*ptr, false).await;
                        prefix = prefix_rest;
                    } else {
                        return None;
                    }
                }
            }
        }
    }

    /// Build a one-item node from a non-empty `Item` (port of `createNodeFromItem`).
    fn create_node_from_item(item: &Item) -> Node {
        match item {
            Item::Empty => empty_node(),
            Item::Leaf { prefix, value } => {
                assert!(!prefix.is_empty(), "LeafPrefix should be non empty.");
                let mut node = empty_node();
                node[prefix.head() as usize] = Item::Leaf {
                    prefix: prefix.tail(),
                    value: *value,
                };
                node
            }
            Item::NodePtr { prefix, ptr } => {
                assert!(!prefix.is_empty(), "NodePtrPrefix should be non empty.");
                let mut node = empty_node();
                node[prefix.head() as usize] = Item::NodePtr {
                    prefix: prefix.tail(),
                    ptr: *ptr,
                };
                node
            }
        }
    }

    /// Optimize and save a node, creating the item that points to it (port of
    /// `saveNodeAndCreateItem`).
    fn save_node_and_create_item(&self, node: &Node, prefix: &KeySegment, compaction: bool) -> Item {
        if compaction {
            let mut non_empty: Vec<(usize, &Item)> = Vec::new();
            for (idx, item) in node.iter().enumerate() {
                if item != &Item::Empty {
                    non_empty.push((idx, item));
                    if non_empty.len() == 2 {
                        break;
                    }
                }
            }
            match non_empty.len() {
                0 => Item::Empty,
                1 => {
                    let (idx, item) = non_empty[0];
                    let idx_seg = KeySegment::new(vec![idx as u8]);
                    match item {
                        Item::Empty => Item::Empty,
                        Item::Leaf {
                            prefix: leaf_prefix,
                            value,
                        } => Item::Leaf {
                            prefix: prefix.concat(&idx_seg).concat(leaf_prefix),
                            value: *value,
                        },
                        Item::NodePtr {
                            prefix: ptr_prefix,
                            ptr,
                        } => Item::NodePtr {
                            prefix: prefix.concat(&idx_seg).concat(ptr_prefix),
                            ptr: *ptr,
                        },
                    }
                }
                _ => Item::NodePtr {
                    prefix: prefix.clone(),
                    ptr: self.save_node(node),
                },
            }
        } else {
            Item::NodePtr {
                prefix: prefix.clone(),
                ptr: self.save_node(node),
            }
        }
    }

    /// Construct a node from an item, loading the child for an empty-prefix `NodePtr` (port of
    /// `constructNodeFromItem`).
    async fn construct_node_from_item(&self, item: &Item) -> Node {
        match item {
            Item::NodePtr { prefix, ptr } if prefix.is_empty() => {
                self.load_node(*ptr, false).await
            }
            _ => Self::create_node_from_item(item),
        }
    }

    /// Insert a leaf into the subtree rooted at `cur_item` (port of `update`).
    #[async_recursion]
    async fn update(
        &self,
        cur_item: Item,
        ins_prefix: KeySegment,
        ins_value: Blake2b256Hash,
    ) -> Option<Item> {
        match cur_item {
            Item::Empty => Some(Item::Leaf {
                prefix: ins_prefix,
                value: ins_value,
            }),
            Item::Leaf {
                prefix: leaf_prefix,
                value: leaf_value,
            } => {
                assert_eq!(
                    leaf_prefix.len(),
                    ins_prefix.len(),
                    "The length of all prefixes in the subtree must be the same."
                );
                if leaf_prefix == ins_prefix {
                    if ins_value == leaf_value {
                        None
                    } else {
                        Some(Item::Leaf {
                            prefix: ins_prefix,
                            value: ins_value,
                        })
                    }
                } else {
                    let (comm_prefix, ins_prefix_rest, leaf_prefix_rest) =
                        KeySegment::common_prefix(&ins_prefix, &leaf_prefix);
                    let mut new_node = empty_node();
                    new_node[leaf_prefix_rest.head() as usize] = Item::Leaf {
                        prefix: leaf_prefix_rest.tail(),
                        value: leaf_value,
                    };
                    new_node[ins_prefix_rest.head() as usize] = Item::Leaf {
                        prefix: ins_prefix_rest.tail(),
                        value: ins_value,
                    };
                    Some(self.save_node_and_create_item(&new_node, &comm_prefix, false))
                }
            }
            Item::NodePtr {
                prefix: ptr_prefix,
                ptr,
            } => {
                assert!(
                    ptr_prefix.len() < ins_prefix.len(),
                    "Radix key should be longer than NodePtr key."
                );
                let (comm_prefix, ins_prefix_rest, ptr_prefix_rest) =
                    KeySegment::common_prefix(&ins_prefix, &ptr_prefix);
                if ptr_prefix_rest.is_empty() {
                    let child_node = self.load_node(ptr, false).await;
                    let child_item_idx = ins_prefix_rest.head() as usize;
                    let child_ins_prefix = ins_prefix_rest.tail();
                    let child_item = child_node[child_item_idx].clone();
                    let child_item_opt =
                        self.update(child_item, child_ins_prefix, ins_value).await;
                    child_item_opt.map(|new_child_item| {
                        let mut updated_child_node = child_node.clone();
                        updated_child_node[child_item_idx] = new_child_item;
                        self.save_node_and_create_item(&updated_child_node, &comm_prefix, false)
                    })
                } else {
                    let mut new_node = empty_node();
                    new_node[ptr_prefix_rest.head() as usize] = Item::NodePtr {
                        prefix: ptr_prefix_rest.tail(),
                        ptr,
                    };
                    new_node[ins_prefix_rest.head() as usize] = Item::Leaf {
                        prefix: ins_prefix_rest.tail(),
                        value: ins_value,
                    };
                    Some(self.save_node_and_create_item(&new_node, &comm_prefix, false))
                }
            }
        }
    }

    /// Delete a leaf from the subtree rooted at `cur_item` (port of `delete`).
    #[async_recursion]
    async fn delete(&self, cur_item: Item, del_prefix: KeySegment) -> Option<Item> {
        match cur_item {
            Item::Empty => None,
            Item::Leaf {
                prefix: leaf_prefix, ..
            } => {
                if leaf_prefix == del_prefix {
                    Some(Item::Empty)
                } else {
                    None
                }
            }
            Item::NodePtr {
                prefix: ptr_prefix,
                ptr,
            } => {
                let (comm_prefix, del_prefix_rest, ptr_prefix_rest) =
                    KeySegment::common_prefix(&del_prefix, &ptr_prefix);
                if !ptr_prefix_rest.is_empty() || del_prefix_rest.is_empty() {
                    None
                } else {
                    let child_node = self.load_node(ptr, false).await;
                    let del_item_idx = del_prefix_rest.head() as usize;
                    let del_item_prefix = del_prefix_rest.tail();
                    let child_item = child_node[del_item_idx].clone();
                    let child_item_opt = self.delete(child_item, del_item_prefix).await;
                    child_item_opt.map(|new_child_item| {
                        let mut new_child_node = child_node.clone();
                        new_child_node[del_item_idx] = new_child_item;
                        self.save_node_and_create_item(&new_child_node, &comm_prefix, true)
                    })
                }
            }
        }
    }

    /// Apply a batch of `HistoryAction`s to a subtree (port of `makeActions`).
    #[async_recursion]
    async fn make_actions(&self, cur_node: &Node, actions: &[HistoryAction]) -> Option<Node> {
        // Group actions by the first byte of their key.
        let mut grouped: Vec<(u8, Vec<HistoryAction>)> = Vec::new();
        for action in actions {
            let first = action.key().head();
            match grouped.iter_mut().find(|(b, _)| *b == first) {
                Some((_, list)) => list.push(action.clone()),
                None => grouped.push((first, vec![action.clone()])),
            }
        }

        let mut new_group_items: Vec<(usize, Option<Item>)> = Vec::new();
        for (group_idx, actions_in_group) in grouped {
            let item_idx = group_idx as usize;
            let item = cur_node[item_idx].clone();
            let result = if actions_in_group.len() == 1 {
                let action = &actions_in_group[0];
                let new_item = match action {
                    HistoryAction::Insert { key, hash } => {
                        self.update(item, key.tail(), *hash).await
                    }
                    HistoryAction::Delete { key } => self.delete(item, key.tail()).await,
                };
                (item_idx, new_item)
            } else {
                let has_insert = actions_in_group
                    .iter()
                    .any(|a| matches!(a, HistoryAction::Insert { .. }));
                let cleared = if item == Item::Empty && !has_insert {
                    Vec::new()
                } else {
                    actions_in_group.clone()
                };
                if cleared.is_empty() {
                    (item_idx, None)
                } else {
                    let created_node = self.construct_node_from_item(&item).await;
                    let new_actions: Vec<HistoryAction> =
                        cleared.iter().map(|a| a.trim()).collect();
                    let new_node_opt = self.make_actions(&created_node, &new_actions).await;
                    let new_item = new_node_opt
                        .map(|n| self.save_node_and_create_item(&n, &KeySegment::empty(), true));
                    (item_idx, new_item)
                }
            };
            new_group_items.push(result);
        }

        let mut new_cur_node = cur_node.clone();
        for (idx, new_item_opt) in new_group_items {
            if let Some(item) = new_item_opt {
                new_cur_node[idx] = item;
            }
        }
        if new_cur_node != *cur_node {
            Some(new_cur_node)
        } else {
            None
        }
    }

    /// Apply actions to a subtree, persist, and return the new root (port of `saveAndCommit`).
    pub async fn save_and_commit(
        &self,
        root_node: &Node,
        actions: &[HistoryAction],
    ) -> Result<Option<(Node, Blake2b256Hash)>, String> {
        let result = match self.make_actions(root_node, actions).await {
            Some(new_root_node) => {
                let new_root_hash = self.save_node(&new_root_node);
                self.commit().await?;
                Some((new_root_node, new_root_hash))
            }
            None => None,
        };
        self.clear_write_cache();
        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_root_is_content_addressed_and_stable() {
        let root = empty_root_hash();
        assert_eq!(root, empty_root_hash());
        // hash of the empty node's canonical encoding
        assert_eq!(root, Blake2b256Hash::create(&encode(&empty_node())));
    }

    #[test]
    fn node_encoding_round_trips() {
        let mut node = empty_node();
        node[1] = Item::Leaf {
            prefix: KeySegment::new(vec![0xff]),
            value: Blake2b256Hash::from_bytes([0x11; 32]),
        };
        node[2] = Item::NodePtr {
            prefix: KeySegment::new(vec![]),
            ptr: Blake2b256Hash::from_bytes([0x22; 32]),
        };
        let decoded = decode(&SerializedNode::try_from(encode(&node).as_slice()).unwrap());
        assert_eq!(decoded, node);
        // node hash is deterministic
        assert_eq!(hash_node(&node).0, hash_node(&decoded).0);
    }

    #[test]
    fn empty_node_encoding_is_empty() {
        assert!(encode(&empty_node()).is_empty());
        assert_eq!(
            decode(&SerializedNode::try_from(&[][..]).unwrap()),
            empty_node()
        );
    }

    fn in_memory_tree() -> RadixTreeImpl {
        use rchain_shared::store::{InMemoryKeyValueStore, KeyValueStore};
        use rchain_shared::typed_store::{BytesCodec, KeyValueTypedStoreCodec};

        let shared: rchain_shared::typed_store::SharedStore = Arc::new(tokio::sync::Mutex::new(
            Box::new(InMemoryKeyValueStore::default()) as Box<dyn KeyValueStore + Send + Sync>,
        ));
        let typed = Arc::new(KeyValueTypedStoreCodec::new(
            shared,
            Arc::new(crate::history::codecs::Blake2b256HashCodec),
            Arc::new(BytesCodec),
        ));
        RadixTreeImpl::new(typed)
    }

    #[tokio::test]
    async fn insert_read_update_delete_round_trip() {
        let tree = in_memory_tree();
        let root = empty_node();
        let key = KeySegment::new(vec![1, 2, 3]);
        let value = Blake2b256Hash::from_bytes([0x42; 32]);

        let (root1, _) = tree
            .save_and_commit(&root, &[HistoryAction::Insert { key: key.clone(), hash: value }])
            .await
            .unwrap()
            .unwrap();
        assert_eq!(tree.read(&root1, &key).await, Some(value));

        // update
        let value2 = Blake2b256Hash::from_bytes([0x43; 32]);
        let (root2, _) = tree
            .save_and_commit(&root1, &[HistoryAction::Insert { key: key.clone(), hash: value2 }])
            .await
            .unwrap()
            .unwrap();
        assert_eq!(tree.read(&root2, &key).await, Some(value2));

        // delete returns to the empty root
        let (root3, _) = tree
            .save_and_commit(&root2, &[HistoryAction::Delete { key: key.clone() }])
            .await
            .unwrap()
            .unwrap();
        assert_eq!(tree.read(&root3, &key).await, None);
        assert_eq!(root3, empty_node());
    }
}
