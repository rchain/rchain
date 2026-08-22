//! The de Bruijn-level environment. Mirrors `rholang/.../interpreter/Env.scala`.

use std::collections::BTreeMap;

/// A de Bruijn environment mapping levels to terms (the Scala `Env[A]`).
///
/// `level` is the current binding level, `shift` offsets the level lookup. `put` binds a value at
/// the current level; `get(k)` resolves de Bruijn index `k` via `(level + shift) - k - 1`.
#[derive(Clone, Debug)]
pub struct Env<A> {
    env_map: BTreeMap<i32, A>,
    level: i32,
    shift: i32,
}

impl<A: Clone> Env<A> {
    pub fn new() -> Self {
        Env {
            env_map: BTreeMap::new(),
            level: 0,
            shift: 0,
        }
    }

    /// Bind `a` at the current level and advance the level (the Scala `put`).
    pub fn put(&self, a: A) -> Self {
        let mut env_map = self.env_map.clone();
        env_map.insert(self.level, a);
        Env {
            env_map,
            level: self.level + 1,
            shift: self.shift,
        }
    }

    /// Resolve de Bruijn index `k` (the Scala `get`).
    pub fn get(&self, k: i32) -> Option<A> {
        self.env_map.get(&((self.level + self.shift) - k - 1)).cloned()
    }

    /// Offset the level lookup by `j` (the Scala `shift`).
    pub fn shift(&self, j: i32) -> Self {
        Env {
            env_map: self.env_map.clone(),
            level: self.level,
            shift: self.shift + j,
        }
    }

    /// The current shift offset (the Scala `env.shift`).
    pub fn shift_amount(&self) -> i32 {
        self.shift
    }

    /// Build an environment from a sequence of values (the Scala `makeEnv`).
    pub fn make_env(values: impl IntoIterator<Item = A>) -> Self {
        let mut env = Env::new();
        for v in values {
            env = env.put(v);
        }
        env
    }
}

impl<A: Clone> Default for Env<A> {
    fn default() -> Self {
        Self::new()
    }
}
