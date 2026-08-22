//! Machine-verifiable DAG edges (port of `api/MachineVerifiableDag.scala`).

use std::future::Future;

use rchain_models::block_hash::BlockHash;

/// A DAG edge (port of `VerifiableEdge`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VerifiableEdge {
    pub from: String,
    pub to: String,
}

impl VerifiableEdge {
    /// The `"from to"` rendering (port of `Show[VerifiableEdge]`).
    pub fn show(&self) -> String {
        format!("{} {}", self.from, self.to)
    }
}

/// Build the verifiable edges from a topo-sort + parent lookup (port of `MachineVerifiableDag.apply`).
pub async fn machine_verifiable_dag<F, Fut>(
    toposort: &[Vec<BlockHash>],
    fetch_parents: F,
) -> Result<Vec<VerifiableEdge>, String>
where
    F: Fn(BlockHash) -> Fut,
    Fut: Future<Output = Result<Vec<BlockHash>, String>>,
{
    // The Scala `foldM` prepends each layer's edges, so the result is reversed topological order.
    let mut acc: Vec<VerifiableEdge> = Vec::new();
    for layer in toposort {
        let mut layer_edges = Vec::new();
        for block_hash in layer {
            let parents = fetch_parents(*block_hash).await?;
            for parent in parents {
                layer_edges.push(VerifiableEdge {
                    from: block_hash.to_hex(),
                    to: parent.to_hex(),
                });
            }
        }
        layer_edges.extend(acc);
        acc = layer_edges;
    }
    Ok(acc)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn hash(byte: u8) -> BlockHash {
        BlockHash::new([byte; 32])
    }

    #[test]
    fn edge_shows_from_to() {
        let edge = VerifiableEdge {
            from: "a".to_string(),
            to: "b".to_string(),
        };
        assert_eq!(edge.show(), "a b");
    }

    #[tokio::test]
    async fn builds_edges_from_toposort() {
        let toposort = vec![vec![hash(1), hash(2)]];
        let edges = machine_verifiable_dag(&toposort, |h| {
            std::future::ready(Ok::<Vec<BlockHash>, String>(vec![hash(h.as_bytes()[0] + 10)]))
        })
        .await
        .unwrap();
        assert_eq!(edges.len(), 2);
        assert_eq!(edges[0].from, hash(1).to_hex());
        assert_eq!(edges[0].to, hash(11).to_hex());
    }
}
